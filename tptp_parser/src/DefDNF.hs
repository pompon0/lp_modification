{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DefDNF where

import Lib
import Pred
import qualified Form
import Form(predNames,funNames,allocName)
import DNF
import qualified NNF

import Data.Maybe(fromJust)
import qualified Data.Set as Set
import Control.Lens hiding(ix)
import qualified Control.Monad.Trans.State.Lazy as StateM

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

orForm'term :: Traversal' OrForm Term
orForm'term = orForm'andClauses.traverse.andClause'atoms.traverse.atom'term

data Stack = Stack {
  _vars :: [VarName],
  _existVars :: [VarName]
}
makeLenses ''Stack

data State = State {
  _nameIndex :: Form.NameIndex,
  _nextVar :: VarName
}
makeLenses ''State

type M = StateM.State State

alloc x = (x,x+1)

term'var :: Traversal' Term VarName
term'var f (unwrap -> TFun fn args) = pure (wrap . TFun fn) <*> (traverse.term'var) f args
term'var f (unwrap -> TVar vn) = pure (wrap . TVar) <*> f vn

pred'args :: Traversal' Pred [Term]
pred'args = pred'spred.spred'args

relevantVars :: Stack -> OrForm -> [Term]
relevantVars s f = Set.toList (Set.intersection sub sup) & traverse %~ (wrap . TVar) where
  sub = Set.fromList (f^..orForm'term.term'var)
  sup = Set.fromList (s^.existVars)

defDNF :: NNF.Form -> Form.NameIndex -> (DNF.OrForm,Form.NameIndex)
defDNF f ni = (dnf,state'^.nameIndex) where
  (dnf,state') = StateM.runState (_defDNF f (Stack [] [])) (State ni 0)

_defDNF :: NNF.Form -> Stack -> M DNF.OrForm
_defDNF nnf s = case nnf of
  NNF.Forall f -> do
    vn <- nextVar %%= alloc
    f' <- _defDNF f (s & vars %~ (vn:))
    let rv = relevantVars s f'
    skolemTerm <- (nameIndex.funNames %%= allocName "_skolem" (length rv)) >>= (\fn -> return $ wrap (TFun fn rv))
    return $ f' & orForm'term.term'subst %~ (\x -> if x==vn then skolemTerm else wrap (TVar x))
  NNF.Exists f -> do
    vn <- nextVar %%= alloc
    _defDNF f (s & vars %~ (vn:) & existVars %~ (vn:))
  NNF.And conj -> do
    conj' <- mapM (\f -> _defDNF f s) conj
    return (mconcat conj')
  NNF.Or [] -> return mempty
  NNF.Or (h:t) -> do
    t' <- _defDNF (NNF.Or t) s
    h' <- _defDNF h s
    case (h', t') of {
      (OrForm [],_) -> return mempty;
      (_,OrForm []) -> return mempty;
      (OrForm [a], OrForm [b]) -> return $ OrForm [a <> b];
      (u,v) -> do {
        let { rv = relevantVars s (u <> v) };
        defPred <- (nameIndex.predNames %%= allocName "_def" (length rv)) >>= (\pn -> return (wrap $ PCustom pn rv));
        let { u' = u & orForm'andClauses.traverse %~ (andClause'atoms %~ (Atom True defPred:)) };
        let { v' = v & orForm'andClauses.traverse %~ (andClause'atoms %~ (Atom False defPred:)) };
        return (u' <> v');
      };
    }
  NNF.Atom sign p -> return $ OrForm [AndClause [
    Atom sign (p & pred'args.traverse.term'subst %~ (\i -> wrap $ TVar (fromJust $ s^.vars.ix i)))]]
