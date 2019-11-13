{-# LANGUAGE ScopedTypeVariables #-}
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

pred'arity :: Fold Pred (PredName,Int)
pred'arity g p = case unwrap p of
  PCustom pn args -> g (pn,length args) *> pure p
  _ -> pure p

orForm'pred :: Traversal' OrForm Pred
orForm'pred = orForm'andClauses.traverse.andClause'atoms.traverse.atom'pred
orForm'term :: Traversal' OrForm Term
orForm'term = orForm'pred.pred'spred.spred'args.traverse

term'arity :: Fold Term (FunName,Int)
term'arity g t = case unwrap t of
  TFun fn args -> g (fn,length args) *> pure t
  _ -> pure t

pred'peq :: Traversal' Pred (Term,Term)
pred'peq f p = case unwrap p of
  PEq x y -> pure (\(x',y') -> wrap $ PEq x' y') <*> f (x,y)
  _ -> pure p

pred'pcustom :: Traversal' Pred (PredName,[Term])
pred'pcustom f p = case unwrap p of
  PCustom pn args -> pure (\(pn',args') -> wrap $ PCustom pn' args') <*> f (pn,args)
  _ -> pure p

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
  ([p], [(pn',a')]) -> case unwrap p of
    PCustom pn a -> if pn==pn' && isSubRelation (zip a a') (c^..posPred.pred'peq) then Just pn else Nothing
    _ -> Nothing
  _ -> Nothing

isFunCongAxiom :: AndClause -> Maybe FunName
isFunCongAxiom c = do
  p <- case (c^..negPred, c^..posPred.pred'pcustom) of { ([p],[]) -> return p; _ -> Nothing }
  (t,t') <- case unwrap p of { PEq t t' -> return (t,t'); _ -> Nothing }
  (fn,a) <- case unwrap t of { TFun fn a -> return (fn,a); _ -> Nothing }
  (fn',a') <- case unwrap t' of { TFun fn' a' -> return (fn',a'); _ -> Nothing }
  if fn==fn' && isSubRelation (zip a a') (c^..posPred.pred'peq) then Just fn else Nothing

{-appendEqAxioms :: OrForm -> OrForm
appendEqAxioms f = let {
    [x,y,z] = labels'ids ["X","Y","Z"];
    reflAxiom = OrClause [eq x x];
    symmAxiom = OrClause [neq x y, eq y x];
    transAxiom = OrClause [neq x y, neq y z, eq x z];
    congPred :: (PredName,Int) -> NotAndForm; -- A 0..c  $0=$i and p($1..$c) => p($1..$0..$c)
    congPred (n,c) = let {
      pred :: [Int] -> Pred;
      pred l = wrap $ PCustom n (map (wrap . TVar . fromIntegral) l);
      (x,y) = ([0..c-1],[c..2*c-1]);
    } in NotAndForm $ [OrClause $ map (\(a,b) -> neq a b) (zip x y) <> [Atom False (pred x), Atom True (pred y)]];
    congFun :: (FunName,Int) -> NotAndForm; -- A 0..c  $0=$i => f($1..$c)=f($1..$0..$c)
    congFun (n,c) = let {
      term :: [Int] -> Term;
      term l = wrap $ TFun n (map (wrap . TVar . fromIntegral) l);
      (x,y) = ([0..c-1],[c..2*c-1]);
    } in NotAndForm $ [OrClause $ map (\(a,b) -> neq a b) (zip x y) <> [Atom True (wrap $ PEq (term x) (term y))]];
    congPredClauses :: NotAndForm = mconcat $ map congPred $ unique $ f^..orForm'pred.pred'arity;
    congFunClauses :: NotAndForm = mconcat $ map congFun $ unique $ f^..orForm'term.term'subterm.term'arity;
  } in f <> notAndForm'orForm (NotAndForm [reflAxiom,symmAxiom,transAxiom] <> congPredClauses <> congFunClauses)
-}

isEqAxiom :: AndClause -> Bool
isEqAxiom c = isReflAxiom c || isSymmAxiom c || isTransAxiom c || isPredCongAxiom c /= Nothing || isFunCongAxiom c /= Nothing

isReflAxiom c = case c of
  AndClause [Atom False p] -> case unwrap p of
    PEq a b -> a==b
    _ -> False
  _ -> False

isSymmAxiom c = case c of
  AndClause [Atom s p, Atom s' p'] -> case (unwrap p, unwrap p') of
    (PEq a b, PEq b' a') -> s/=s' && a==a' && b==b'
    _ -> False
  _ -> False

isTransAxiom c = case (c^..negPred,c^..posPred.pred'pcustom) of
  ([p],[]) -> case unwrap p of 
    PEq a1 a2 -> any (\(l,(b1,b2),r) -> isSubRelation [(a1,b1),(a2,b2)] (l<>r)) $ select $ c^..posPred.pred'peq
    _ -> False
  _ -> False


