{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Skolem(Form(..), skol) where

import Lib
import Pred
import Control.Lens (makeLenses, (^.), (%~), (.~), over, view, use, (.=), (%=))
import qualified Data.Map as Map
import qualified NNF as F
import Data.List(intercalate)
import qualified Data.Set.Monad as Set
import Control.Monad(join)
import qualified Control.Monad.Trans.State.Lazy as StateM
import Control.Lens(makeLenses,Traversal,Traversal',Fold,Lens,Lens',Iso',dimap)

data Form = And [Form]
  | Or [Form]
  | Atom Bool Pred
  deriving(Eq)

instance ShowCtx Form where
  show (And x) = fmap (printf "and(%s)") (sepList x)
  show (Or x) = fmap (printf "or(%s)") (sepList x)
  show (Atom s p) = fmap ((if s then "+" else "-")++) (showCtx p)

--------------------------------------

skol :: (Ctx,F.Form) -> (Ctx,Form)
skol (ctx,f) = skol'Form emptyValuation ctx f

with :: s -> (StateM.State a) -> (s,a)
with = flip StateM.runState
mapWithState :: (a -> (s -> (b,s))) -> ([a],s) -> ([b],s)
mapWithState f (l,s) = StateM.runState (mapM (StateM.state . f) l) s

skol'Form :: Valuation -> (Ctx,F.Form) -> (Ctx,Form)
skol'Form val (ctx,f) = case f of
  F.Forall vn x -> skol'Form (val & vn ?~ c) (ctx',x) where
    isVar t = case unwrap t of { TVar _->True; _-> False }
    (fn,ctx') = (ctx & fromFunNames %%~ alloc)
    c = wrap $ TFun fn (filter isVar st)
  F.Exists vn x -> skol'Form (val & vn ?~ v) ctx x where
    v = wrap $ TVar vn
  F.Or l -> (ctx', Or l') where
    (l',ctx') = mapWithState (swap.skol'Form val.swap) (l,ctx)
  F.And l -> (ctx', And l') where
    (l',ctx') = mapWithState (swap.skol'Form val.swap) (l,ctx)
  F.Atom s p -> (ctx, Atom s skol'Pred val (ctx,p))

skol'Pred :: Valuation -> (Ctx,Pred) -> Pred
skol'Pred val (ctx,p) = p & wh.pred'args %~ (\t -> skol'Term (ctx,t))

skolT t = case unwrap t of
  TVar vn -> do
    mt <- use (varStack.ix vn)
    case mt of
      Nothing -> fail "oob"
      Just t -> return t
  TFun name args -> do
    n <- lookupFunName name
    a <- mapM skolT args
    return (wrap $ TFun n a)

{-
Amodel Ey Ax f <=>
Amodel Ax(y) Ey f
  for every choice of counter examples x
  exists y for which x(y) is not a counterexample

psimplify1
  Not False ->  True
  Not True -> False
  Not (Not p) = p
  And p False = False
  And False p = False
  And p True = p
  And True p = p
  Or p False = p
  Or False p = p
  Or p True = True
  Or True p = True
  Imp False p = True
  Imp p True = True
  Imp True p = p
  Imp p False = Not p
  Iff p True = p
  Iff True p = p
  Iff p False = Not p
  Iff False p = Not p
  ? = ?
psimplify fm = {apply psimplify1 bottom up on the subexpressions}

simplify1
  Forall x p = x\in p ? Forall x p : p
  Exists x p = x\in p ? Exists x p : p

simplify fm = {apply simplify1 bottom up on the subexpressions}

nnf (And p q) = And (nnf p) (nnf q)
nnf (Or p q) = Or (nnf p) (nnf q)
nnf (Imp p q) = Or (nnf (not p)) (nnf q)
nnf (Iff p q) = Or (And (nnf p) (nnf q)) (And (nnf (Not p)) (nnf (Not q)))
nnf (Not (Not p)) = nnf p
nnf (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))
nnf (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))
nnf (Not (Imp p q)) = And (nnf p) (nnf (Not q))
nnf (Not (Iff p q)) = Or (And (nnf p) (Not (nnf q))) (And (Not (nnf p)) (nnf q))
... forall/exists
nnf ? = ?

nnf2 = nnf . psimplify

skolem (Exists y p) = 
  vars = {free vars in p} - {y}
  return $ skolem (replace y in p with f_y(vars))
skolem (Forall x p) = return $ Forall x (skolem p)
skolem (And p q) = and (skolem p) (skolem q)
skolem (Or p q) = or (skolem p) (skolem q)
skolem ? = ?

generalize fm = {prepend Forall quantifiers for all free vars of fm}

askolemize fm = skolem (nnf2 (simplify fm))
tab fm = tableau [askolemize $ Not $ generalize fm]

purednf (And p q) = {sum all pairs from (purednf p) and (purednf q)}
purednf (Or p q) = {sum (purednf p) and (purednf q)}
purednf ? = [[?]]
purecnf fm = {negation of purednf (nnf (Not fm))}

list_conj l = {Connect all literals in list l with And }
simpdnf fm = {nontrivial, not subsumed elements of purednf (nnf fm)}
simpcnf fm = {nontrivial, not subsumed elements of purecnf (fm)} // nnf can be skipped, because it is applied inside purecnf

specialize fm = {drop leading Forall quantifiers}
prenex fm = {run pullquants bottom-up on subexpressions of fm}


pnf fm = prenex (nnf (simplify fm))
pure_resolution fm = resloop {used=[], unused=simpcnf(specialize(pnf fm))}
resolution fm = map (pure_resolution ** list_conj) (simpdnf $ askolemize $ Not $ generalize fm)

exponential blowup:
  removing <=>
  conversion to DNF

-}
