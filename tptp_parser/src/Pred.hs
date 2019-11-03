{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Pred where

import Ctx
import HashSeq
import qualified Coq
import Control.Lens(makeLenses,Traversal,Traversal',Fold,Lens,Lens',Iso',dimap)
import Data.List(intercalate)
import qualified Data.Map as Map
import HashSeq
import Control.Lens
import Data.Maybe(fromJust)
import Text.Printf

data Term' = TVar VarName | TFun FunName [Term]
data Pred' = PEq Term Term | PCustom PredName [Term]

type Term = WithHash Term'
type Pred = WithHash Pred'

instance HashSeq Term' where
  hashSeq (TVar vn) = [unit 0, unit (vn^._Wrapped'.uuid.to fromIntegral)]
  hashSeq (TFun fn args) = unit 1 : unit (fn^._Wrapped'.uuid.to fromIntegral) : map hash args

instance HashSeq Pred' where
  hashSeq (PEq a b) = [unit 0,hash a,hash b]
  hashSeq (PCustom pn args) = unit 1 : unit (pn^._Wrapped'.uuid.to fromIntegral) : map hash args

term'subst :: Traversal Term Term VarName Term
term'subst g (unwrap -> TVar vn) = g vn
term'subst g (unwrap -> TFun fn args) = wrap.TFun fn <$> (traverse.term'subst) g args

term'varName'rec :: Traversal' Term VarName
term'varName'rec = wh.(\g t -> case t of {
    TVar vn -> TVar <$> g vn;
    TFun fn args -> TFun fn <$> (traverse.term'varName'rec) g args;
  })

term'subterm :: Fold Term Term
term'subterm g t@(unwrap -> TFun fn args) = (traverse.term'subterm) g args *> g t *> pure t
term'subterm g t@(unwrap -> TVar _) = g t *> pure t

data SPred = SPred { _spred'name :: PredName, _spred'args :: [Term] }
makeLenses ''SPred

makeSPred :: Pred -> SPred
makeSPred (unwrap -> PEq l r) = SPred eqPredName [l,r]
makeSPred (unwrap -> PCustom pn args) = SPred pn args

makePred :: SPred -> Pred
makePred (SPred pn args) = wrap $ case args of
  [l,r] | pn == eqPredName -> PEq l r
  _ -> PCustom pn args 

pred'spred :: Iso' Pred SPred 
pred'spred = dimap makeSPred (fmap makePred)

pred'name :: Lens' Pred PredName
pred'name = pred'spred.spred'name

pred'args :: Lens' Pred [Term]
pred'args = pred'spred.spred'args

instance Show Term where
  show (unwrap -> TVar vn) = show vn
  show (unwrap -> TFun fn args) = printf "%s(%s)" (show fn) (intercalate "," $ map show args)  

instance Show Pred where
  show p = printf "%s(%s)" (show $ p^.pred'name) (intercalate "," $ p^..pred'args.traverse.to show)


----------------------------------------------------

-- Valuation is a function V-FV -> T[FV], represented by acyclic V-FV -> T[V] function
type Valuation = Map.Map VarName Term

emptyValuation = Map.empty

-- function T[V] -> T[FV], represented by the valuation
eval :: Valuation -> Term -> Term
eval s t@(unwrap -> TVar vn) = case Map.lookup vn s of { Nothing -> t; Just t' -> eval s t' }
eval s (unwrap -> TFun f args) = wrap $ TFun f (map (eval s) args)

ground :: Term -> Term
ground (unwrap -> TVar _) = wrap $ TFun extraConstName []
ground (unwrap -> TFun f args) = wrap $ TFun f (map ground args)

term'coq :: Term -> Coq.Expr
term'coq (unwrap -> TVar vn) = Coq.Value (show vn)
term'coq (unwrap -> TFun fn args) = foldl Coq.Apply (Coq.Value (show fn)) (map term'coq args)
