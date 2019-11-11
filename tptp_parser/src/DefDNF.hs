{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DefDNF (nnf'dnf, nnf'dnf'def) where

import HashSeq
import Pred
import DNF
import Ctx
import qualified NNF
import qualified Data.Set as Set
import Control.Lens 
import Text.Printf

{-
 - rename variables
 -
 - calculate DNF for each subformula
 - and when encountering forall, substitute
 - the variable for a skolem term depending on
 - all existential variables from the context
 - which occur in any of the clauses.
 -
 - Track along the non-definitional DNF form
 - If it is simpler than definitional, replace it
 - If it is duplicated, replace definitional form with the previous
 -}

{- valid extension
 - if original environment is a prefix of the new one and all new variables have different
 - names than the ones from the original env, then new environment is a valid extension of
 - the old one.
 - It means that you can replace the original environment with the new one without renaming
 - variables in the term. It will be useful when evaluating a beta reduction - when traversing
 - the lambda term looking for variables to replace, you keep track of the current environment
 - making sure that it is a valid extension of the lambda environment (by introducing renaming
 - in the current environment). This way you don't have to traverse the argument term during
 - beta reduction
 -}

orForm'varName'rec :: Traversal' OrForm VarName
orForm'varName'rec = orForm'andClauses.traverse.andClause'atoms.traverse.atom'pred.pred'args.traverse.term'varName'rec

data Local = Local {
  _global :: Global,
  _existsVars' :: Stack VarName,
  _valuation :: FlatValuation
}
makeLenses ''Local
local'empty = Local global'empty stack'empty emptyValuation

relevantVars :: Stack VarName -> OrForm -> Set.Set VarName
relevantVars s dnf = Set.intersection sub sup where
  sub = Set.fromList (dnf^..orForm'varName'rec)
  sup = stack'values s

type Join = Global -> OrForm -> OrForm -> (Global,OrForm)

----------------------------------------------

nnf'dnf'def :: (Global,NNF.NNF) -> (GlobalVar,OrForm)
nnf'dnf'def (g,nnf) = (gv,f) where
  (g',f) = defDNF (local'empty & global .~ g) nnf
  gv = GlobalVar {
    _global' = g',
    _existsVars = stack'make (Set.fromList $ f^..orForm'varName'rec)
  }

defDNFs :: Local -> Join -> OrForm -> [NNF.NNF] -> (Global,OrForm)
defDNFs local join zero = rec (local^.global)  where
  rec g [] = (g,zero)
  rec g (h:t) = join g3 h' t' where
    (g2,t') = rec g t :: (Global,OrForm)
    (g3,h') = defDNF (local & global .~ g2) h :: (Global,OrForm)

push'skolemFunName = stack'push'unused (FunName . printf "skolem%d")
push'defPredName = stack'push'unused (PredName . printf "def%d")

defDNF :: Local -> NNF.NNF -> (Global,OrForm)
defDNF local nnf = case nnf of
  NNF.Forall vn f -> defDNF local3 f where
    ev = local^.existsVars'.stack'ids
    (fn,local2) = local & global.funs %%~ push'skolemFunName
    local3 = local2 & valuation.at vn ?~ wrap (TFun fn (map (wrap.TVar) ev))
  NNF.Exists vn f -> defDNF local3 f where
    (vn',local2) = local & existsVars' %%~ stack'push'unused (VarName . printf "%s%d" (show vn))
    local3 = local2 & valuation.at vn ?~ wrap (TVar vn')
  NNF.Or conj -> defDNFs local (\gv da db -> (gv, da <> db)) mempty conj
  NNF.And disj -> defDNFs local join (OrForm [mempty]) disj where
    join g da db = case (da, db) of
      (OrForm [],_) -> (g,mempty);
      (_,OrForm []) -> (g,mempty);
      (OrForm [a], OrForm [b]) ->(g, OrForm [a <> b]);
      (_,_) -> (g',da' <> db') where
        (pn,g') = g & preds %%~ stack'push'unused (PredName . printf "_def%d")
        rv = relevantVars (local^.existsVars') (da <> db)
        defPred = wrap $ PCustom pn $ map (wrap.TVar) (Set.toList rv)
        da' = da & orForm'andClauses.traverse.andClause'atoms %~ (Atom True defPred:)
        db' = db & orForm'andClauses.traverse.andClause'atoms %~ (Atom False defPred:)
  NNF.Atom sign p -> (local^.global, OrForm [AndClause [Atom sign p']]) where
    p' = p & pred'args.traverse.term'subst %~ eval (local^.valuation)

-------------------------------------------------

nnf'dnf :: (Global,NNF.NNF) -> (GlobalVar,OrForm)
nnf'dnf (g,nnf) = (gv,f) where
  (g',f) = dnf (local'empty & global .~ g) nnf
  gv = GlobalVar {
    _global' = g',
    _existsVars = stack'make (Set.fromList $ f^..orForm'varName'rec)
  }

dnfs :: Local -> Join -> OrForm -> [NNF.NNF] -> (Global,OrForm)
dnfs local join zero = rec (local^.global)  where
  rec g [] = (g,zero)
  rec g (h:t) = join g3 h' t' where
    (g2,t') = rec g t :: (Global,OrForm)
    (g3,h') = dnf (local & global .~ g2) h :: (Global,OrForm)

dnf :: Local -> NNF.NNF -> (Global,OrForm)
dnf local nnf = case nnf of
  NNF.Forall vn f -> dnf local3 f where
    ev = local^.existsVars'.stack'ids
    (fn,local2) = local & global.funs %%~ push'skolemFunName 
    local3 = local2 & valuation.at vn ?~ wrap (TFun fn (map (wrap.TVar) ev))
  NNF.Exists vn f -> dnf local3 f where
    (vn',local2) = local & existsVars' %%~ stack'push'unused (VarName . printf "%s%d" (show vn))
    local3 = local2 & valuation.at vn ?~ wrap (TVar vn')
  NNF.Or conj -> dnfs local (\gv da db -> (gv, da <> db)) mempty conj
  NNF.And disj -> dnfs local join (OrForm [mempty]) disj where
    join = \g (OrForm a) (OrForm b) -> (g, OrForm [x <> y | x <- a, y <- b])
  NNF.Atom sign p -> (local^.global, OrForm [AndClause [Atom sign p']]) where
    p' = p & pred'args.traverse.term'subst %~ eval (local^.valuation)
