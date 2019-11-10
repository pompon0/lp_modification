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
    local0 = stack'empty;
    vn = VarName "X";
    local1 = stack'push vn local0;
    refl = NNF.Atom True$ wrap $ PEq (wrap $ TVar vn) (wrap $ TVar vn);
    nnf = lambda NNF.Forall local0 (local1,refl); -- A[X] X=X
    (gv,dnf) = nnf'dnf'def (global'empty,nnf);
    [fn] = gv^.global'.funs.stack'ids;
    val = emptyValuation & at vn ?~ (wrap $ TFun fn []);
    (gv',dnf') = nnf'dnf (gv^.global', refl & NNF.nnf'pred.pred'args.traverse.term'subst %~ eval val);
  }
  dnf @?= dnf'
  gv @?= gv'

