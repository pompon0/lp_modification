module Main where

import Test.Tasty (testGroup,defaultMain)
import Test.Tasty.HUnit (assertFailure,testCase,assertBool,(@?=))

import Ctx
import HashSeq
import Pred
import DNF
import DefDNF
import Parser
import qualified NNF
import qualified FOF 
import Control.Lens
import Control.Monad
import Text.Printf

main = defaultMain tests
tests = testGroup "DefDNFTest.hs" [
  testCase "simpleForall" testSimpleForall,
  testCase "nnf'dnf :: noVarsInSkolemTerms" $ testNoVarsInSkolemTerms nnf'dnf,
  testCase "nnf'dnf'def :: noVarsInSkolemTerms" $ testNoVarsInSkolemTerms nnf'dnf'def]

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


l143_zfmisc_1 = "\
  \fof(l143_zfmisc_1, conjecture,  (! [A] :  (! [B] :  (! [C] :  ~ ( ( ~ (r2_hidden(A, C))  &  ( ~ (r2_hidden(B, C))  &  ~ (r1_xboole_0(k2_tarski(A, B), C)) ) ) ) ) ) ) ).\
  \fof(t51_zfmisc_1, axiom,  (! [A] :  (! [B] :  (! [C] :  ~ ( ( ~ (r2_hidden(A, B))  &  ( ~ (r2_hidden(C, B))  &  ~ (r1_xboole_0(k2_tarski(A, C), B)) ) ) ) ) ) ) )."

orForm'subterm :: Fold OrForm Term
orForm'subterm = orForm'andClauses.traverse.andClause'atoms.traverse.atom'args.traverse.term'subterm

testNoVarsInSkolemTerms nnf'dnf = forM_ (dnf^..orForm'subterm) expected where
  Err (Right file) = Parser.parse l143_zfmisc_1
  g = FOF.global'make [file]
  Err (Right fof) = FOF.fromProto'File g file
  (gv,dnf) = nnf'dnf (g, NNF.fof'nnf fof)
  expected t = case unwrap t of {
    -- skolem terms should have no vars
    TFun fn args -> when (not (stack'has fn (g^.funs)) && args /= []) $
      assertFailure (printf "unexpected skolem term %s" (show t));
    TVar _ -> return ();
  }
