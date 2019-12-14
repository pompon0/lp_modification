{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
module DNF where

import Pred
import qualified MGU
import qualified Proto.Tptp as T

import Ctx
import qualified FOF
import TptpLens
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad(foldM,when)
import Control.Lens
import qualified Data.List as List
import Data.ProtoLens(defMessage)
import Tptp(formula'assert)

data Atom = Atom { _atom'sign :: Bool, _atom'pred :: Pred } deriving(Eq,Ord)
makeLenses ''Atom

instance Show Atom where
  show (Atom s p) = (if s then "+" else "-") ++ show p

atom'name :: Lens' Atom PredName
atom'name = atom'pred.pred'spred.spred'name
atom'args :: Lens' Atom [Term]
atom'args = atom'pred.pred'spred.spred'args

opposite :: Atom -> Atom -> Bool
opposite a1 a2 = a1^.atom'sign /= a2^.atom'sign && a1^.atom'name == a2^.atom'name

-- negated Conjunctive Normal Form
newtype OrClause = OrClause { _orClause'atoms :: [Atom] } deriving(Ord,Eq,Semigroup,Monoid)
makeLenses ''OrClause
instance Show OrClause where { show c = List.intercalate " \\/ " $ map show (c^.orClause'atoms) }

newtype NotAndForm = NotAndForm { _notAndForm'orClauses :: [OrClause] } deriving(Ord,Eq,Semigroup,Monoid)
makeLenses ''NotAndForm
instance Show NotAndForm where { show f = unlines $ map show $ f^.notAndForm'orClauses }

-- Disjunctive Normal Form
newtype AndClause = AndClause { _andClause'atoms :: [Atom] } deriving(Ord,Eq,Semigroup,Monoid)
makeLenses ''AndClause
instance Show AndClause where { show c = List.intercalate " /\\ " $ map show (c^.andClause'atoms) }

newtype OrForm = OrForm { _orForm'andClauses :: [AndClause] } deriving(Ord,Eq,Semigroup,Monoid)
makeLenses ''OrForm
instance Show OrForm where { show f = unlines $ map show $ f^.orForm'andClauses }

orForm'notAndForm :: OrForm -> NotAndForm
orForm'notAndForm (OrForm cla) = NotAndForm (map notAndClause cla)

notAndForm'orForm :: NotAndForm -> OrForm
notAndForm'orForm (NotAndForm cla) = OrForm (map notOrClause cla)

notOrClause (OrClause atoms) = AndClause (atoms & traverse.atom'sign %~ not)
notAndClause (AndClause atoms) = OrClause (atoms & traverse.atom'sign %~ not)

filterSign :: Bool -> Set.Set Atom -> Set.Set Pred
filterSign s = Set.map (^.atom'pred) . Set.filter (\a -> a^.atom'sign == s)

sumOr x y = x <> y
prodOr (OrForm fa) (OrForm fb) = OrForm [ca <> cb | ca <- fa, cb <- fb]
  
simplify :: OrForm -> OrForm
simplify (OrForm x) =
  let
    normalized = Set.fromList $ x^..traverse.andClause'atoms.to Set.fromList
    nonTrivial  = Set.filter (\c -> Set.empty == Set.intersection (filterSign True c) (filterSign False c)) normalized
    notSubsumed = Set.filter (\c -> not $ any (\d -> d /= c && Set.isSubsetOf d c) nonTrivial) nonTrivial
  in OrForm $ Set.toAscList $ Set.map (AndClause . Set.toAscList) notSubsumed

isSubForm :: OrForm -> OrForm -> Maybe [AndClause]
isSubForm a b = mapM (\c -> List.find (isInstance c) (b^.orForm'andClauses)) (a^.orForm'andClauses)

atom'mgu :: (Atom,Atom) -> MGU.Valuation -> Maybe MGU.Valuation
atom'mgu (a1,a2) val = do
  when ((a1^.atom'sign) /= (a2^.atom'sign)) Nothing
  when ((a1^.atom'name) /= (a2^.atom'name)) Nothing
  when (length (a1^.atom'args) /= length (a2^.atom'args)) Nothing
  foldM (flip MGU.term'mgu) val $ zip (a1^.atom'args) (a2^.atom'args)

andClause'mgu :: (AndClause,AndClause) -> MGU.Valuation -> Maybe MGU.Valuation
andClause'mgu (c1,c2) val = do
  when (length (c1^.andClause'atoms) /= length (c2^.andClause'atoms)) Nothing
  foldM (flip atom'mgu) val $ zip (c1^.andClause'atoms) (c2^.andClause'atoms) 

isInstance :: AndClause -> AndClause -> Bool
isInstance a b = andClause'mgu (a,b) MGU.empty /= Nothing

-----------------------------------------------------

fromProto'File :: GlobalVar -> T.File -> Err OrForm
fromProto'File gv f = do
  i' <- mapM (fromProto'Input gv) (f^. #input)
  return $ OrForm $ i'^..traverse.traverse.to notOrClause

fromProto'Input :: GlobalVar -> T.Input -> Err (Maybe OrClause)
fromProto'Input gv i = do
  case i^. #language of { T.Input'CNF -> return (); }
  case i^. #role of {
    T.Input'AXIOM -> return ();
    T.Input'PLAIN -> return ();
    T.Input'NEGATED_CONJECTURE -> return ();
  }
  fromProto'Form gv (i^. #formula.formula'assert)

isOp :: T.Formula'Operator'Type -> T.Formula'Formula -> Bool 
isOp o f = case f of { T.Formula'Op op -> (op^. #type') == o; _ -> False}

fromProto'Form :: GlobalVar -> T.Formula'Formula -> Err (Maybe OrClause)
fromProto'Form gv f = case f of
  T.Formula'Pred' _ -> (\a -> return $ Just $ OrClause [a]) =<< fromProto'Atom gv f
  T.Formula'Op op -> case (op^. #type') of
    T.Formula'Operator'OR ->
      if any (isOp T.Formula'Operator'TRUE) (op^.. #args.traverse.formula'assert) then return Nothing
      else (Just . OrClause) <$> mapM (fromProto'Atom gv) (op^.. #args.traverse.formula'assert.filtered (not . isOp T.Formula'Operator'FALSE))
    T.Formula'Operator'FALSE -> return $ Just $ OrClause []
    T.Formula'Operator'TRUE -> return Nothing
    T.Formula'Operator'NEG -> (\a -> return $ Just $ OrClause [a]) =<< fromProto'Atom gv f


fromProto'Atom :: GlobalVar -> T.Formula'Formula -> Err Atom
fromProto'Atom gv f = case f of
  T.Formula'Op op -> do
    case op^. #type' of {
      T.Formula'Operator'NEG -> do
        [f'] <- mapM FOF.getFormula (op^. #args)
        a <- fromProto'Atom gv f'
        return (a & atom'sign %~ not)
    }
  T.Formula'Pred' pred -> Atom True <$> FOF.fromProto'Pred (FOF.Local (gv^.global') (gv^.existsVars)) pred

toProto'File :: OrForm -> T.File
toProto'File (OrForm clauses) = defMessage & #input .~ map (toProto'Input . notAndClause) clauses

toProto'Input :: OrClause -> T.Input
toProto'Input (OrClause atoms) = defMessage
  & #language .~ T.Input'CNF
  & #role .~ T.Input'PLAIN
  & #formula. #maybe'formula ?~ (T.Formula'Op $ defMessage
    & #type' .~ T.Formula'Operator'OR
    & #args .~ map (\a -> defMessage & #maybe'formula ?~ toProto'Atom a) atoms)

toProto'Atom :: Atom -> T.Formula'Formula
toProto'Atom (Atom sign pred) = if sign then pos else neg where
  pos = T.Formula'Pred' (FOF.toProto'Pred pred)
  neg = T.Formula'Op (defMessage
    & #type' .~ T.Formula'Operator'NEG
    & #args .~ [defMessage & #maybe'formula ?~ pos])

