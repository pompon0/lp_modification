{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module FOF where

import Ctx
import HashSeq
import Pred hiding (pred'args,pred'name,term'varName'rec)
import qualified Proto.Tptp as T
import TptpLens

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Control.Monad(when)
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Labels()
import Text.Printf
import Debug.Trace

data FOF = Forall VarName FOF
  | Exists VarName FOF
  | And [FOF]
  | Or [FOF]
  | Xor FOF FOF
  | Neg FOF
  | Atom Pred
  deriving(Eq)

instance Show FOF where
  show (Forall vn f) = printf "A[%s] %s" (show vn) (show f)
  show (Exists vn f) = printf "E[%s] %s" (show vn) (show f)
  show (And x) = printf "and(%s)" (List.intercalate "," $ map show x)
  show (Or x) = printf "or(%s)" (List.intercalate "," $ map show x)
  show (Xor l r) = printf "xor(%s,%s)" (show l) (show r)
  show (Neg f) = printf "-%s" (show f)
  show (Atom p) = show p

---------------------------------------

freeVars :: T.Formula'Formula -> Set.Set String
freeVars f = case f of
  T.Formula'Pred' pred -> Set.fromList $ pred^..pred'args.traverse.term'varName'rec
  T.Formula'Quant' quant -> Set.difference (Set.unions $ quant^..quant'sub.to freeVars) (Set.fromList $ quant^..quant'varName)
  T.Formula'Op op -> Set.unions (op^.. #args.traverse. #maybe'formula.traverse.to freeVars)

global'make :: [T.File] -> Global
global'make fs = Global {
    _funs = stack'make (Set.fromList $ fs^..traverse.file'formula.formula'pred.pred'args.traverse.term'funName'rec.to FunName),
    _preds = stack'make (Set.fromList $ fs^..traverse.file'formula.formula'pred.pred'name.to PredName)
  }

globalVar'make :: [T.File] -> GlobalVar 
globalVar'make fs = GlobalVar {
    _global' = global'make fs,
    _existsVars = stack'make (Set.map VarName $ Set.unions $ fs^..traverse.file'formula.to freeVars)
  }


getFormula :: T.Formula -> Err T.Formula'Formula
getFormula f = case f^. #maybe'formula of
  Nothing -> fail "formula missing"
  Just x -> return x

-----------------------------------

data Local = Local {
  _global :: Global,
  _vars :: Stack VarName
}
makeLenses ''Local

local'empty = Local global'empty stack'empty

fromProto'File :: Global -> T.File -> Err FOF
fromProto'File glob f = Or <$> mapM (input'fof glob) (f^. #input)

input'fof :: Global -> T.Input -> Err FOF
input'fof glob i = do
  f <- getFormula (i^. #formula)
  let vars = stack'make $ Set.map VarName $ freeVars f
  case i^. #language of {
    T.Input'CNF -> return ();
    T.Input'FOF -> when (vars/=stack'empty) $ fail "unexpected free vars in FOF";
    lang -> fail ("unexpected language: " ++ show lang);
  };
  f2 <- formula'fof (Local glob vars) f
  let f3 = lambda Forall stack'empty (vars,f2)
  case i^. #role of {
    T.Input'AXIOM -> return (Neg f3);
    T.Input'LEMMA -> return (Neg f3);
    T.Input'PLAIN -> return (Neg f3);
    T.Input'DEFINITION -> return (Neg f3);
    T.Input'NEGATED_CONJECTURE -> return (Neg f3);
    T.Input'CONJECTURE -> return f3; 
    T.Input'HYPOTHESIS -> return f3;
  };

formula'fof :: Local -> T.Formula'Formula -> Err FOF
formula'fof local f =
  case f of 
    T.Formula'Pred' pred -> Atom <$> fromProto'Pred local pred
    T.Formula'Quant' quant -> do { 
      let { local' = local & vars %~ stack'push' (map VarName $ quant^..quant'varName) };
      w <- (case (quant^. #type') of
        T.Formula'Quant'FORALL -> return Forall
        T.Formula'Quant'EXISTS -> return Exists);
      sub <- formula'fof local' =<< getFormula (quant^. #sub);
      return $ lambda w (local^.vars) (local'^.vars,sub);
    }
    T.Formula'Op op -> let
      args' = mapM (\a -> getFormula a >>= formula'fof local) (op^. #args) in
      case (op^. #type') of
        T.Formula'Operator'NEG -> do { [a] <- args'; return (Neg a) } -- will throw Exception
        T.Formula'Operator'OR -> Or <$> args'
        T.Formula'Operator'AND -> And <$> args'
        T.Formula'Operator'IFF -> do { [l,r] <- args'; return (Neg (Xor l r)) }
        T.Formula'Operator'IMPL -> do { [l,r] <- args'; return (Or [(Neg l),r]) }
        T.Formula'Operator'XOR -> do { [l,r] <- args'; return (Xor l r) }
        T.Formula'Operator'NOR -> Neg . Or <$> args'
        T.Formula'Operator'NAND -> Neg . And <$> args'
        T.Formula'Operator'TRUE -> return (And [])
        T.Formula'Operator'FALSE -> return (Or [])

fromProto'Pred :: Local -> T.Formula'Pred -> Err Pred
fromProto'Pred local pred =
  let args' = mapM (fromProto'Term local) (pred^.pred'args) in
  case (pred^. #type') of
    T.Formula'Pred'CUSTOM -> wrap.PCustom (stack'find (local^.global.preds) (PredName $ pred^.pred'name)) <$> args'
    T.Formula'Pred'EQ -> do { [l,r] <- args'; return (wrap $ PEq l r) }

fromProto'Term :: Local -> T.Term -> Err Term
fromProto'Term local term =
  case (term^. #type') of
    T.Term'VAR -> return $ wrap $ TVar $ stack'find (local^.vars) (VarName $ term^.term'name) 
    T.Term'EXP -> wrap.TFun fn <$> args' where
      args' = mapM (fromProto'Term local) (term^. #args)
      fn = stack'find (local^.global.funs) (FunName $ term^.term'name) 

toProto'Pred :: Pred -> T.Formula'Pred
toProto'Pred pred = case unwrap pred of
  PEq l r -> defMessage & #type' .~ T.Formula'Pred'EQ & #args .~ map toProto'Term [l,r]
  PCustom pn args -> defMessage & #type' .~ T.Formula'Pred'CUSTOM & pred'name .~ show pn & #args .~ map toProto'Term args

toProto'Term :: Term -> T.Term
toProto'Term term = case unwrap term of
  TVar vn -> defMessage & #type' .~ T.Term'VAR & term'name .~ show vn
  TFun fn args -> defMessage & #type' .~ T.Term'EXP & term'name .~ show fn & #args .~ map toProto'Term args

