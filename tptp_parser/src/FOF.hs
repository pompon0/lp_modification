{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module FOF where

import Err
import Ctx
import HashSeq
import Pred hiding (pred'args,pred'name,term'varName'rec)
import qualified Proto.Tptp as T
import Tptp

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
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

freeVars :: NodeTree -> Set.Set Node
freeVars (NodeTree n args) = runIdentity $ do
  fv <- Set.empty & for args (\a cont s -> cont (Set.union s (freeVars a)))
  r$ case n^.type_ of
    T.FORALL -> let [v,_] = args in Set.difference fv (freeVars v)
    T.EXISTS -> let [v,_] = args in Set.difference fv (freeVars v)
    T.TERM_VAR -> let [] = args in Set.singleton n
    _ -> fv

-----------------------------------

fromProto'File :: T.File -> Err FOF
fromProto'File f = do
  idx <- nodes'index (f^. #nodes)
  fs <- [] & for (f^. #input) (\i cont [] -> do
    f <- input'fof idx i
    ft <- cont []
    r$ f:ft)
  r$ Or fs

input'fof :: NodeIndex -> T.Input -> Err FOF
input'fof idx i = do
  (f,[]) <- stream'tree idx (i^. #formula)
  fv <-r$ freeVars f
  case i^. #language of {
    T.Input'CNF -> return ();
    T.Input'FOF -> when (not $ Set.null fv) $ fail "unexpected free vars in FOF";
    lang -> fail ("unexpected language: " ++ show lang);
  };
  f <- formula'fof f
  f <- f & for fv (\v cont f -> cont (Forall (VarName v) f))
  case i^. #role of {
    T.Input'AXIOM -> r (Neg f);
    T.Input'LEMMA -> r (Neg f);
    T.Input'PLAIN -> r (Neg f);
    T.Input'DEFINITION -> r (Neg f);
    T.Input'NEGATED_CONJECTURE -> r (Neg f);
    T.Input'CONJECTURE -> r f; 
    T.Input'HYPOTHESIS -> r f;
  }

formula'fof :: NodeTree -> Err FOF
formula'fof nt@(NodeTree n args) = do
  quant <-r$ \q -> do
    [v,f] <-r$ args
    f <- formula'fof f;
    r$ q (VarName (v^.root)) f;
  args'fof <-r$ \args -> [] & for args (\a cont [] -> do
    a <- formula'fof a
    at <- cont []
    r$ a:at)
  case n^.type_ of 
    T.PRED -> Atom <$> fromProto'Pred nt
    T.PRED_EQ -> Atom <$> fromProto'Pred nt
    T.FORALL -> quant Forall
    T.EXISTS -> quant Exists
    T.FORM_NEG -> do { [a] <- args'fof args; r$ Neg a }
    T.FORM_OR -> Or <$> args'fof args
    T.FORM_AND -> And <$> args'fof args
    T.FORM_IFF -> do { [x,y] <- args'fof args; r$ Neg (Xor x y) }
    T.FORM_IMPL -> do { [x,y] <- args'fof args; r$ Or [(Neg x),y] }
    T.FORM_XOR -> do { [x,y] <- args'fof args; r$ Xor x y }
    T.FORM_NOR -> Neg <$> Or <$> args'fof args
    T.FORM_NAND -> Neg <$> And <$> args'fof args
    T.FORM_TRUE -> r$ And []
    T.FORM_FALSE -> r$ Or []

isPred :: Node -> Bool
isPred n = case n^.type_ of
  T.PRED -> True
  T.PRED_EQ -> True
  _ -> False

isTerm :: Node -> Bool
isTerm n = case n^.type_ of
  T.TERM_FUN -> True
  T.TERM_VAR -> True
  _ -> False

fromProto'Pred :: NodeTree -> Err Pred
fromProto'Pred (NodeTree n args) = do
  when (not $ isPred n) (fail "non-pred node")
  args <- [] & for args (\a cont [] -> do
    a <- fromProto'Term a
    at <- cont []
    r$ a:at)
  r$ wrap$ Pred (PredName n) args

fromProto'Term :: NodeTree -> Err Term
fromProto'Term (NodeTree n args) = do
  when (not $ isTerm n) (fail "non-term node")
  args <- [] & for args (\a cont [] -> do
    a <- fromProto'Term a
    at <- cont []
    r$ a:at)
  r$ wrap$ case n^.type_ of
    T.TERM_VAR -> TVar (VarName n) 
    T.TERM_FUN -> TFun (FunName n) args

toProto'Pred :: Pred -> NodeTree
toProto'Pred (unwrap -> Pred (PredName n) args) = NodeTree n (toProto'Term <$> args)

toProto'Term :: Term -> NodeTree
toProto'Term (unwrap -> TVar (VarName n)) = NodeTree n []
toProto'Term (unwrap -> TFun (FunName n) args) = NodeTree n (toProto'Term <$> args)

