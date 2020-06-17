{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module EqAxioms(
  isEqAxiom,
  isReflAxiom, isSymmAxiom, isTransAxiom,
  isPredCongAxiom, isFunCongAxiom,
) where

import HashSeq
import Ctx
import Pred
import DNF
import Control.Lens
import qualified Data.Set as Set
unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

select :: [a] -> [([a],a,[a])]
select [] = []
select (h:t) = ([],h,t) : map (\(l,x,r) -> (h:l,x,r)) (select t)


--eq l r = Atom True (wrap $ PEq (wrap $ TVar $ fromIntegral l) (wrap $ TVar $ fromIntegral r));
--neq l r = Atom False (wrap $ PEq (wrap $ TVar $ fromIntegral l) (wrap $ TVar $ fromIntegral r));

orForm'pred :: Traversal' OrForm Pred
orForm'pred = orForm'andClauses.traverse.andClause'atoms.traverse.atom'pred
orForm'term :: Traversal' OrForm Term
orForm'term = orForm'pred.pred'args.traverse

term'arity :: Fold Term (FunName,Int)
term'arity g t = case unwrap t of
  TFun fn args -> g (fn,length args) *> pure t
  _ -> pure t

pred'peq :: Traversal' Pred (Term,Term)
pred'peq f p = if isEq (p^.pred'name)
  then p & pred'args (\[x,y] -> (\(x,y) -> [x,y]) <$> f (x,y))
  else pure p

pred'pcustom :: Traversal' Pred (PredName,[Term])
pred'pcustom f p = if not (isEq (p^.pred'name))
  then (\(n,a)-> p & pred'args.~a & pred'name.~n) <$> f (p^.pred'name,p^.pred'args)
  else pure p

posPred :: Fold AndClause Pred
posPred = andClause'atoms.traverse.filtered (^.atom'sign).atom'pred

negPred :: Fold AndClause Pred
negPred = andClause'atoms.traverse.filtered (not.(^.atom'sign)).atom'pred

isSubRelation :: (Eq a, Ord a) => [(a,a)] -> [(a,a)] -> Bool
isSubRelation a b =
  let norm r = Set.fromList [if x<y then (x,y) else (y,x) | (x,y) <- r, x/=y]
  in Set.isSubsetOf (norm a) (norm b)

isPredCongAxiom :: AndClause -> Maybe PredName
isPredCongAxiom c = case (c^..negPred, c^..posPred.pred'pcustom) of
  ([unwrap -> Pred pn a], [(pn',a')]) ->
    if pn==pn' && isSubRelation (zip a a') (c^..posPred.pred'peq) then Just pn else Nothing
  _ -> Nothing

isFunCongAxiom :: AndClause -> Maybe FunName
isFunCongAxiom c = do
  p <- case (c^..negPred, c^..posPred.pred'pcustom) of { ([p],[]) -> return p; _ -> Nothing }
  (t,t') <- case unwrap p of { Pred pn [t,t'] | isEq pn -> return (t,t'); _ -> Nothing }
  (fn,a) <- case unwrap t of { TFun fn a -> return (fn,a); _ -> Nothing }
  (fn',a') <- case unwrap t' of { TFun fn' a' -> return (fn',a'); _ -> Nothing }
  if fn==fn' && isSubRelation (zip a a') (c^..posPred.pred'peq) then Just fn else Nothing

isEqAxiom :: AndClause -> Bool
isEqAxiom c = isReflAxiom c || isSymmAxiom c || isTransAxiom c || isPredCongAxiom c /= Nothing || isFunCongAxiom c /= Nothing

isReflAxiom c = case c of
  AndClause [Atom False p] -> case unwrap p of
    Pred pn [a,b] | isEq pn -> a==b
    _ -> False
  _ -> False

isSymmAxiom c = case c of
  AndClause [Atom s p, Atom s' p'] -> case (unwrap p, unwrap p') of
    (Pred pn [a,b], Pred pn' [b',a']) | isEq pn && isEq pn' -> s/=s' && a==a' && b==b'
    _ -> False
  _ -> False

isTransAxiom c = case (c^..negPred,c^..posPred.pred'pcustom) of
  ([p],[]) -> case unwrap p of 
    Pred pn [a1,a2] | isEq pn -> any (\(l,(b1,b2),r) -> isSubRelation [(a1,b1),(a2,b2)] (l<>r)) $ select $ c^..posPred.pred'peq
    _ -> False
  _ -> False


