module Main where

import Test.Tasty (testGroup,defaultMain)
import Test.Tasty.HUnit (Assertion,testCase,(@=?))

import HashSeq
import Ctx
import Pred
import qualified MGU
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

main = defaultMain tests
tests = testGroup "MGUTest" [testCase "loop" loopTest]

loopTest = do
  let {
    s0 = emptyValuation;
    [vn0,vn1] = labels'ids ["X","Y"];
    s1 = fromJust $ MGU.runMGU (wrap $ TVar vn0, wrap $ TVar vn0) s0;
    s2 = fromJust $ MGU.runMGU (wrap $ TVar vn0, wrap $ TVar vn1) s1;
  }
  [(vn1, wrap $ TVar vn0)] @=? Map.toList s2
