{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Proof where

import Err
import Ctx
import DNF
import Pred
import Tptp
import qualified Data.Map as Map
import Data.Text.Lens
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Labels()
import Control.Lens
import Data.List(partition,group,sort)
import Data.Either(partitionEithers)
import Valid
import EqAxioms
import qualified Proto.Solutions as SPB
import Text.Printf

classify :: NodeIndex -> OrForm -> OrForm -> Err SPB.Stats
classify idx (OrForm c) f = do
  (idx,standardNodes) <-r$ index'withStandard idx
  (refl,c) <-r$ partition isReflAxiom c;
  (symm,c) <-r$ partition isSymmAxiom c;
  (trans,c) <-r$ partition isTransAxiom c;
  (fmono,c) <-r$ partitionEithers (map (\c -> case isFunCongAxiom c of { Just fn -> Left fn; Nothing -> Right c }) c);
  (pmono,c) <-r$ partitionEithers (map (\c -> case isPredCongAxiom c of { Just pn -> Left pn; Nothing -> Right c }) c);
  x <- isSubForm (OrForm c) f ??? "proof doesn't imply the formula"
  let {
    funMono = (flip map) (group $ sort fmono) (\fns -> (defMessage :: SPB.Stats'FunMono)
      & #name.unpacked .~ show (head fns)
      & #count .~ fromIntegral (length fns));
    predMono = (flip map) (group $ sort pmono) (\pns -> (defMessage :: SPB.Stats'PredMono)
      & #name.unpacked .~ show (head pns)
      & #count .~ fromIntegral (length pns));
    orClauses = (flip map) (group $ sort x) (\l -> (defMessage :: SPB.Stats'OrClause)
      & #cla .~ toProto'Input standardNodes (notAndClause $ head l)
      & #count .~ fromIntegral (length l));
  }
  return $ defMessage
    & #refl .~ fromIntegral (length refl)
    & #symm .~ fromIntegral (length symm)
    & #trans .~ fromIntegral (length trans)
    & #funMono .~ funMono 
    & #predMono .~ predMono 
    & #orClauses .~ orClauses
    & #nodes .~ index'nodes idx
