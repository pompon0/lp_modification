{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Proof where

import Lib
import DNF
import Pred
import qualified Proto.Proof as P
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Labels()
import Control.Lens
import Control.Monad(foldM,forM)
import Control.Lens()
import Data.Monoid(Endo(..))
import Data.Functor.Const(Const(..))
import Data.List(partition,group,sort)
import Data.Either(partitionEithers)
import Valid(counterExample)
import EqAxioms
import Form
import qualified Proto.Solutions as SPB

import qualified Control.Monad.Trans.Except as ExceptM

type Proof = OrForm

-----------------------------------------------------

val'lookup :: Valuation -> VarName -> Term
val'lookup val vn = case Map.lookup vn val of { Just t -> t; Nothing -> wrap $ TVar vn }

andClause'subst :: Traversal AndClause AndClause VarName Term
andClause'subst = andClause'atoms.traverse.atom'pred.pred'spred.spred'args.traverse.term'subst

andClause'term :: Traversal' AndClause Term
andClause'term = andClause'atoms.traverse.atom'args.traverse

-----------------------------------------------------

type E = ExceptM.Except String

classify :: Ctx -> Proof -> OrForm -> E SPB.Stats
classify ctx (OrForm c0) f = do
  let {
  (refl,c1) = partition isReflAxiom c0;
  (symm,c2) = partition isSymmAxiom c1;
  (trans,c3) = partition isTransAxiom c2;
  (fmono,c4) = partitionEithers (map (\c -> case isFunCongAxiom c of { Just fn -> Left fn; Nothing -> Right c }) c3);
  (pmono,c5) = partitionEithers (map (\c -> case isPredCongAxiom c of { Just pn -> Left pn; Nothing -> Right c }) c4);
  }
  x <- case isSubForm (OrForm c5) f of
    Just x -> return x
    Nothing -> fail "proof doesn't imply the formula"
  funMono <- forM (group $ sort fmono) (\l -> do
    let mfn = ctx^.fromFunNames.at (head l)
    fn <- assertMaybe mfn
    return $ (defMessage :: SPB.Stats'FunMono) & #name .~ Text.pack fn & #count .~ fromIntegral (length l))
  predMono <- forM (group $ sort pmono) (\l -> do
    let mpn = ctx^.fromPredNames.at (head l)
    pn <- assertMaybe mpn
    return $ (defMessage :: SPB.Stats'PredMono) & #name .~ Text.pack pn & #count .~ fromIntegral (length l))
  orClauses <- forM (group $ sort x) (\l -> do
    cla <- toProto'Input ctx (notAndClause $ head l)
    return $ (defMessage :: SPB.Stats'OrClause) & #cla .~ cla & #count .~ fromIntegral (length l))
  return $ defMessage
    & #refl .~ fromIntegral (length refl)
    & #symm .~ fromIntegral (length symm)
    & #trans .~ fromIntegral (length trans)
    & #funMono .~ funMono 
    & #predMono .~ predMono 
    & #orClauses .~ orClauses
 
