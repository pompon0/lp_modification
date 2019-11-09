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
    [x,y] = map (wrap.TVar) $ labels'ids ["X","Y"];
    s = fromJust $ return MGU.empty >>= MGU.term'mgu (y,x) >>= MGU.term'mgu (x,y)
  }
  x @=? MGU.val'get s y
  x @=? MGU.val'get s x
