module Main where

import Test.Tasty (testGroup,defaultMain)
import Test.Tasty.HUnit (assertFailure,testCase,assertBool,(@?=))

import DNF
import qualified NNF as N
import Pred
import Form(emptyNI)
import DefDNF

tests = testGroup "DefDNFTest.hs" [
  testCase "simpleForall" testSimpleForall]

testSimpleForall = do
  let refl = wrap $ PEq (wrap $ TVar 0) (wrap $ TVar 0)
  let nnf = N.Forall $ N.Atom True $ refl
  let (dnf,_) = defDNF nnf emptyNI
  let f0 = wrap $ TFun 0 []
  dnf @?= OrForm [AndClause [Atom True $ wrap $ PEq f0 f0]]

main = defaultMain tests
