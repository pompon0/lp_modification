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

import Err
import Ctx
import qualified FOF
import Tptp
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Lens
import qualified Data.List as List
import Data.ProtoLens(defMessage)
import Text.Printf

data Atom = Atom { _atom'sign :: Bool, _atom'pred :: Pred } deriving(Eq,Ord)
makeLenses ''Atom

instance Show Atom where
  show (Atom s p) = (if s then "+" else "-") ++ show p

atom'name :: Lens' Atom PredName
atom'name = atom'pred.pred'name
atom'args :: Lens' Atom [Term]
atom'args = atom'pred.pred'args

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

isSubForm :: OrForm -> OrForm -> Err [AndClause]
isSubForm a b = [] & for (a^.orForm'andClauses) (\ac cont [] -> do
  h <- assert (List.find (isInstance ac) (b^.orForm'andClauses)) ??? printf "ac=%s b=%s" (show ac) (show (b^.orForm'andClauses))
  t <- cont []
  r$ h:t)

atom'mgu :: (Atom,Atom) -> MGU.Valuation -> Maybe MGU.Valuation
atom'mgu (a1,a2) val = do
  when ((a1^.atom'sign) /= (a2^.atom'sign)) Nothing
  when ((a1^.atom'name) /= (a2^.atom'name)) Nothing
  when (length (a1^.atom'args) /= length (a2^.atom'args)) Nothing
  val & for (zip (a1^.atom'args) (a2^.atom'args)) (\t12 cont val -> cont =<< MGU.term'mgu t12 val)
    

andClause'mgu :: (AndClause,AndClause) -> MGU.Valuation -> Maybe MGU.Valuation
andClause'mgu (c1,c2) val = do
  when (length (c1^.andClause'atoms) /= length (c2^.andClause'atoms)) Nothing
  val & for (zip (c1^.andClause'atoms) (c2^.andClause'atoms)) (\a12 cont val -> cont =<< atom'mgu a12 val)

isInstance :: AndClause -> AndClause -> Bool
isInstance a b = runIdentity $ do
  ok <-r$ andClause'mgu (a,b) MGU.empty /= Nothing
  r$ ok
-----------------------------------------------------

fromProto'File :: T.File -> Err OrForm
fromProto'File f = do
  idx <- nodes'index (f^. #nodes)
  cs <- [] & for (f^. #input) (\i cont [] -> do
    ct <- cont []
    mc <- fromProto'Input idx i
    r$ case mc of { Nothing -> ct; Just c -> notOrClause c:ct })
  r$ OrForm cs

fromProto'Input :: NodeIndex -> T.Input -> Err (Maybe OrClause)
fromProto'Input idx i = do
  case i^. #language of { T.Input'CNF -> return (); }
  case i^. #role of {
    T.Input'AXIOM -> return ();
    T.Input'PLAIN -> return ();
    T.Input'NEGATED_CONJECTURE -> return ();
  }
  (f,[]) <- stream'tree idx (i^. #formula)
  fromProto'Clause f

--isOp :: T.Formula'Operator'Type -> T.Formula'Formula -> Bool 
--isOp o f = case f of { T.Formula'Op op -> (op^. #type') == o; _ -> False}

fromProto'Clause :: NodeTree -> Err (Maybe OrClause)
fromProto'Clause nt@(NodeTree n args) = do
  atom'clause <-r$ \n -> do
    a <- fromProto'Atom n
    r$ Just $ OrClause [a]
  case n^.type_ of
    T.FORM_TRUE -> r$ Nothing
    T.FORM_FALSE -> r$ Just $ OrClause []
    T.PRED_EQ -> atom'clause nt
    T.FORM_NEG -> atom'clause nt
    T.PRED -> atom'clause nt
    T.FORM_OR -> do
      trueArg <- False & for args (\n cont trueArg -> cont (trueArg || n^.root.type_==T.FORM_TRUE))
      if trueArg then r$ Nothing else Just . OrClause <$> ([] & for args (\a cont [] -> do
        at <- cont []
        if a^.root.type_==T.FORM_FALSE then r$ at else do
          a <- fromProto'Atom a
          r$ a:at))

fromProto'Atom :: NodeTree -> Err Atom
fromProto'Atom nt@(NodeTree n args) = case n^.type_ of
  T.FORM_NEG -> do
    [a] <-r$ args
    a <- fromProto'Atom a
    r (a & atom'sign %~ not)
  _ -> Atom True <$> FOF.fromProto'Pred nt

toProto'File :: OrForm -> Err T.File
toProto'File f = do
  preds <-r$ f^..orForm'andClauses.traverse.andClause'atoms.traverse.atom'pred
  idx <- empty'index & for preds (\p cont idx -> cont =<< tree'index (FOF.toProto'Pred p) idx)
  (idx,standardNodes) <-r$ index'withStandard idx
  is <- [] & for (f^.orForm'andClauses) (\c cont [] -> do
    is <- cont []
    r$ toProto'Input standardNodes (notAndClause c) : is)
  r$ defMessage
    & #input .~ is 
    & #nodes .~ index'nodes idx 

toProto'Input :: (T.Type -> Node) -> OrClause -> T.Input
toProto'Input standardNodes cla = defMessage
  & #language .~ T.Input'CNF
  & #role .~ T.Input'PLAIN
  & #formula .~ tree'stream (toProto'orClause standardNodes cla) [] 

toProto'orClause :: (T.Type -> Node) -> OrClause -> NodeTree
toProto'orClause standardNodes (OrClause atoms) =
  NodeTree (standardNodes T.FORM_OR) (toProto'Atom standardNodes <$> atoms)

toProto'Atom :: (T.Type -> Node) -> Atom -> NodeTree
toProto'Atom standardNodes (Atom sign pred) = if sign then pos else neg where
  pos = FOF.toProto'Pred pred
  neg = NodeTree (standardNodes T.FORM_NEG) [pos]

