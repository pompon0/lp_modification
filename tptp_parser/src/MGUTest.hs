{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty (testGroup,defaultMain)
import Test.Tasty.HUnit (Assertion,testCase,(@=?))

import Prelude hiding(id)
import HashSeq
import Ctx
import Err
import Tptp
import Pred
import Control.Lens
import qualified MGU
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Proto.Tptp as T
import Data.Maybe
import Control.Lens

main = defaultMain tests
tests = testGroup "MGUTest" [testCase "loop" loopTest]

var :: NodeIndex -> String -> (Term,NodeIndex)
var idx x = runIdentity $ do
  i <-r$ index'nextID idx
  n <-r$ Node {
    _type_ = T.TERM_VAR,
    _id = i,
    _name = Just x,
    _arity = 0
  }
  r$ (wrap $ TVar $ VarName n, idx & at i .~ Just n)

loopTest :: IO () = do
  ([x,y],_) <- ([],empty'index) & for ["X","Y"] (\x cont ([],idx) -> do
    (vt,idx) <- cont ([],idx)
    (v,idx) <-r$ var idx x
    r$ (v:vt,idx))
  s <-r$ fromJust $ return MGU.empty >>= MGU.term'mgu (y,x) >>= MGU.term'mgu (x,y)
  x @=? MGU.val'get s y
  x @=? MGU.val'get s x
