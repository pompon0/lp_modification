module Main where

import Test.Tasty (testGroup,defaultMain)
import Test.Tasty.HUnit (assertFailure,testCase,assertBool,(@?=))

import Ctx
import HashSeq
import Pred
import DNF
import DefDNF
import qualified NNF
import qualified FOF 
import Control.Lens

main = defaultMain tests
tests = testGroup "DefDNFTest.hs" [
  testCase "simpleForall" testSimpleForall]

testSimpleForall = do
  let {
    local0 = empty'Stack;
    (vn,local1) = push1 "X" local0;
    refl = NNF.Atom True$ wrap $ PEq (wrap $ TVar vn) (wrap $ TVar vn);
    nnf = lambda NNF.Forall local0 (local1,refl); -- A[X] X=X
    (gv,dnf) = nnf'dnf'def (empty'Global,nnf);
    [fn] = gv^.global'.funs.to stack'ids;
    val = emptyValuation & at vn ?~ (wrap $ TFun fn []);
    (gv',dnf') = nnf'dnf (gv^.global', refl & NNF.nnf'pred.pred'args.traverse %~ eval val);
  }
  dnf @?= dnf'
  gv @?= gv'

