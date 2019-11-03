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

{-preds :: Form -> [Pred]
preds f = case f of
  Forall vn x -> preds x
  Exists vn x -> preds x
  Neg x -> preds x
  And x -> join (map preds x)
  Or x -> join (map preds x)
  Xor x y -> preds x ++ preds y
  Atom x -> [x]
-}
---------------------------------------

make'Global :: T.File -> Global
make'Global f = Global {
    _funs = push (Set.fromList $ f^..file'formula.formula'pred.pred'args.traverse.term'funName'rec) empty'Stack,
    _preds = push (Set.fromList $ f^..file'formula.formula'pred.pred'name) empty'Stack
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

empty'Local = Local empty'Global empty'Stack

fromProto'File :: T.File -> Err (Global,FOF)
fromProto'File f = do
  let glob = make'Global f
  f' <- Or <$> mapM (fromProto'Input glob) (f^. #input)
  return (glob,f')

fromProto'Input :: Global -> T.Input -> Err FOF
fromProto'Input glob i = do
  f <- getFormula (i^. #formula)
  let vars = push (freeVars f) empty'Stack
  case i^. #language of {
    T.Input'CNF -> return ();
    T.Input'FOF -> when (vars/=empty'Stack) $ fail "unexpected free vars in FOF";
    lang -> fail ("unexpected language: " ++ show lang);
  };
  f2 <- fromProto'Form (Local glob vars) f
  let f3 = lambda Forall empty'Stack (vars,f2)
  case i^. #role of {
    T.Input'AXIOM -> return (Neg f3);
    T.Input'PLAIN -> return (Neg f3);
    T.Input'NEGATED_CONJECTURE -> return (Neg f3);
    T.Input'CONJECTURE -> return f3; 
  };

fromProto'Form :: Local -> T.Formula'Formula -> Err FOF
fromProto'Form local f =
  case f of 
    T.Formula'Pred' pred -> Atom <$> fromProto'Pred local pred
    T.Formula'Quant' quant -> do { 
      let { local' = local & vars %~ push (Set.fromList $ quant^..quant'varName) };
      w <- (case (quant^. #type') of
        T.Formula'Quant'FORALL -> return Forall
        T.Formula'Quant'EXISTS -> return Exists);
      sub <- fromProto'Form local' =<< getFormula (quant^. #sub);
      return $ lambda w (local^.vars) (local'^.vars,sub);
    }
    T.Formula'Op op -> let
      args' = mapM (\a -> getFormula a >>= fromProto'Form local) (op^. #args) in
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
    T.Formula'Pred'CUSTOM -> wrap.PCustom (find (pred^.pred'name) (local^.global.preds)) <$> args'
    T.Formula'Pred'EQ -> do { [l,r] <- args'; return (wrap $ PEq l r) }

fromProto'Term :: Local -> T.Term -> Err Term
fromProto'Term local term =
  case (term^. #type') of
    T.Term'VAR -> return $ wrap $ TVar $ find (term^.term'name) (local^.vars)
    T.Term'EXP -> wrap.TFun fn <$> args' where
      args' = mapM (fromProto'Term local) (term^. #args)
      fn = find (term^.term'name) (local^.global.funs)

toProto'Pred :: Pred -> T.Formula'Pred
toProto'Pred pred = case unwrap pred of
  PEq l r -> defMessage & #type' .~ T.Formula'Pred'EQ & #args .~ map toProto'Term [l,r]
  PCustom pn args -> defMessage & #type' .~ T.Formula'Pred'CUSTOM & pred'name .~ show pn & #args .~ map toProto'Term args

toProto'Term :: Term -> T.Term
toProto'Term term = case unwrap term of
  TVar vn -> defMessage & #type' .~ T.Term'VAR & term'name .~ show vn
  TFun fn args -> defMessage & #type' .~ T.Term'EXP & term'name .~ show fn & #args .~ map toProto'Term args

freeVars :: T.Formula'Formula -> Set.Set String
freeVars f = case f of
  T.Formula'Pred' pred -> Set.fromList $ pred^..pred'args.traverse.term'varName'rec
  T.Formula'Quant' quant -> Set.difference (Set.unions $ quant^..quant'sub.to freeVars) (Set.fromList $ quant^..quant'varName)
  T.Formula'Op op -> Set.unions (op^.. #args.traverse. #maybe'formula.traverse.to freeVars)

