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

orForm'varName'rec :: Traversal' OrForm VarName
orForm'varName'rec = orForm'andClauses.traverse.andClause'atoms.traverse.atom'pred.pred'args.traverse.term'varName'rec

data Local = Local {
  _globalVar :: GlobalVar,
  _valuation :: FlatValuation
}
makeLenses ''Local
empty'Local = Local empty'GlobalVar emptyValuation

relevantVars :: Stack VarName -> OrForm -> Set.Set VarName
relevantVars s dnf = Set.intersection sub sup where
  sub = Set.fromList (dnf^..orForm'varName'rec)
  sup = Set.fromList (stack'ids s)

type Join = GlobalVar -> OrForm -> OrForm -> (GlobalVar,OrForm)

----------------------------------------------

nnf'dnf'def :: (Global,NNF.NNF) -> (GlobalVar,OrForm)
nnf'dnf'def (g,nnf) = defDNF (empty'Local & globalVar.global' .~ g) nnf

defDNFs :: Local -> Join -> OrForm -> [NNF.NNF] -> (GlobalVar,OrForm)
defDNFs local join zero = rec (local^.globalVar)  where
  rec gv [] = (gv,zero)
  rec gv (h:t) = join gv3 h' t' where
    (gv2,t') = rec gv t :: (GlobalVar,OrForm)
    (gv3,h') = defDNF (local & globalVar .~ gv2) h :: (GlobalVar,OrForm)

defDNF :: Local -> NNF.NNF -> (GlobalVar,OrForm)
defDNF local nnf = case nnf of
  NNF.Forall vn f -> defDNF local3 f where
    ev = local^.globalVar.existsVars.to stack'ids
    (fn,local2) = local & globalVar.global'.funs %%~ push1 "_skolem" :: (FunName,Local)
    local3 = local2 & valuation.at vn ?~ wrap (TFun fn (map (wrap.TVar) ev))
  NNF.Exists vn f -> defDNF local3 f where
    (vn',local2) = local & globalVar.existsVars %%~ reloc vn
    local3 = local2 & valuation.at vn ?~ wrap (TVar vn')
  NNF.Or conj -> defDNFs local (\gv da db -> (gv, da <> db)) mempty conj
  NNF.And disj -> defDNFs local join (OrForm [mempty]) disj where
    join gv da db = case (da, db) of
      (OrForm [],_) -> (gv,mempty);
      (_,OrForm []) -> (gv,mempty);
      (OrForm [a], OrForm [b]) ->(gv, OrForm [a <> b]);
      (_,_) -> (gv',da' <> db') where
        (pn,gv') = gv & global'.preds %%~ push1 "_def"
        rv = relevantVars (gv'^.existsVars) (da <> db)
        defPred = wrap $ PCustom pn $ map (wrap.TVar) (Set.toList rv)
        da' = da & orForm'andClauses.traverse.andClause'atoms %~ (Atom True defPred:)
        db' = db & orForm'andClauses.traverse.andClause'atoms %~ (Atom False defPred:)
  NNF.Atom sign p -> (local^.globalVar, OrForm [AndClause [Atom sign p']]) where
    p' = p & pred'args.traverse.term'subst %~ eval (local^.valuation)

-------------------------------------------------

nnf'dnf :: (Global,NNF.NNF) -> (GlobalVar,OrForm)
nnf'dnf (g,nnf) = dnf (empty'Local & globalVar.global' .~ g) nnf

dnfs :: Local -> Join -> OrForm -> [NNF.NNF] -> (GlobalVar,OrForm)
dnfs local join zero = rec (local^.globalVar)  where
  rec gv [] = (gv,zero)
  rec gv (h:t) = join gv3 h' t' where
    (gv2,t') = rec gv t :: (GlobalVar,OrForm)
    (gv3,h') = dnf (local & globalVar .~ gv2) h :: (GlobalVar,OrForm)

dnf :: Local -> NNF.NNF -> (GlobalVar,OrForm)
dnf local nnf = case nnf of
  NNF.Forall vn f -> dnf local3 f where
    ev = local^.globalVar.existsVars.to stack'ids
    (fn,local2) = local & globalVar.global'.funs %%~ push1 "_skolem" :: (FunName,Local)
    local3 = local2 & valuation.at vn ?~ wrap (TFun fn (map (wrap.TVar) ev))
  NNF.Exists vn f -> dnf local3 f where
    (vn',local2) = local & globalVar.existsVars %%~ reloc vn
    local3 = local2 & valuation.at vn ?~ wrap (TVar vn')
  NNF.Or conj -> dnfs local (\gv da db -> (gv, da <> db)) mempty conj
  NNF.And disj -> dnfs local join (OrForm [mempty]) disj where
    join = \gv (OrForm a) (OrForm b) -> (gv, OrForm [x <> y | x <- a, y <- b])
  NNF.Atom sign p -> (local^.globalVar, OrForm [AndClause [Atom sign p']]) where
    p' = p & pred'args.traverse.term'subst %~ eval (local^.valuation)
