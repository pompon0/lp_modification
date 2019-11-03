{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Proof where

import Ctx
import DNF
import Pred
--import qualified Proto.Proof as P
import qualified Data.Map as Map
import Data.Text.Lens
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Labels()
import Control.Lens
import Control.Monad(foldM,forM)
import Data.List(partition,group,sort)
import Data.Either(partitionEithers)
import Valid
import EqAxioms
--import Form
import qualified Proto.Solutions as SPB

import qualified Control.Monad.Trans.Except as ExceptM

classify :: OrForm -> OrForm -> Err SPB.Stats
classify (OrForm c0) f = do
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
  let {
    funMono = (flip map) (group $ sort fmono) (\fns -> (defMessage :: SPB.Stats'FunMono)
      & #name.unpacked .~ head fns^._Wrapped'.label
      & #count .~ fromIntegral (length fns));
    predMono = (flip map) (group $ sort pmono) (\pns -> (defMessage :: SPB.Stats'PredMono)
      & #name.unpacked .~ head pns^._Wrapped'.label
      & #count .~ fromIntegral (length pns));
    orClauses = (flip map) (group $ sort x) (\l -> (defMessage :: SPB.Stats'OrClause)
      & #cla .~ toProto'Input (notAndClause $ head l)
      & #count .~ fromIntegral (length l));
  }
  return $ defMessage
    & #refl .~ fromIntegral (length refl)
    & #symm .~ fromIntegral (length symm)
    & #trans .~ fromIntegral (length trans)
    & #funMono .~ funMono 
    & #predMono .~ predMono 
    & #orClauses .~ orClauses
 
