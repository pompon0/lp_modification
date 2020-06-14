{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
data Pred' = Pred { _pred'name :: PredName, _pred'args :: [Term] }

type Term = WithHash Term'
type Pred = WithHash Pred'
makeFieldsNoPrefix ''Pred'

instance HasPred'name Pred PredName where
  pred'name = wh.pred'name

instance HasPred'args Pred [Term] where
  pred'args = wh.pred'args


instance HashSeq Term' where
  hashSeq (TVar vn) = [unit 0, hash (wrap vn)]
  hashSeq (TFun fn args) = unit 1 : hash (wrap fn) : (hash <$> args)

instance HashSeq Pred' where
  hashSeq (Pred pn args) = hash (wrap pn) : (hash <$> args)

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

instance Show Term where
  show (unwrap -> TVar vn) = show vn
  show (unwrap -> TFun fn args) = printf "%s(%s)" (show fn) (intercalate "," $ map show args)  

instance Show Pred where
  show p = printf "%s(%s)" (show $ p^.pred'name) (intercalate "," $ p^..pred'args.traverse.to show)


----------------------------------------------------

type FlatValuation = Map.Map VarName Term

emptyValuation = Map.empty
eval :: FlatValuation -> VarName -> Term
eval val vn = val^.at vn.non (error "var not found")

--ground :: Term -> Term
--ground (unwrap -> TVar _) = wrap $ TFun extraConstName []
--ground (unwrap -> TFun f args) = wrap $ TFun f (map ground args)

term'coq :: Term -> Coq.Expr
term'coq (unwrap -> TVar vn) = Coq.Value (show vn)
term'coq (unwrap -> TFun fn args) = foldl Coq.Apply (Coq.Value (show fn)) (map term'coq args)
