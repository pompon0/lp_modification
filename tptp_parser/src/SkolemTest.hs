module SkolemTest(tests) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertFailure,testCase,assertBool,(@?=))

import qualified NNF as N
import qualified Skolem as S
import Pred

tests = testGroup "SkolemTest" [
  testCase "simpleForall" testSimpleForall,
  testCase "simpleExists" testSimpleExists,
  testCase "FE" testFE,
  testCase "EF" testEF]

testSimpleForall = do
  let refl = wrap $ PEq (wrap $ TVar 0) (wrap $ TVar 0)
  let nnf = N.Forall $ N.Atom True $ refl
  S.skol nnf @?= (S.Atom True $ wrap $ PEq (wrap $ TFun 0 []) (wrap $ TFun 0 []))

testSimpleExists = do
  let refl = wrap $ PEq (wrap $ TVar 0) (wrap $ TVar 0)
  let nnf = N.Exists $ N.Atom True $ refl
  S.skol nnf @?= (S.Atom True $ wrap $ PEq (wrap $ TVar 0) (wrap $ TVar 0))

testFE = do
  let refl = wrap $ PEq (wrap $ TVar 0) (wrap $ TVar 1)
  let nnf = N.Forall $ N.Exists $ N.Atom True $ refl
  S.skol nnf @?= (S.Atom True $ wrap $ PEq (wrap $ TVar 0) (wrap $ TFun 0 []))

testEF = do
  let refl = wrap $ PEq (wrap $ TVar 1) (wrap $ TVar 0)
  let nnf = N.Exists $ N.Forall $ N.Atom True $ refl
  S.skol nnf @?= (S.Atom True $ wrap $ PEq (wrap $ TVar 0) (wrap $ TFun 0 [wrap $ TVar 0]))


