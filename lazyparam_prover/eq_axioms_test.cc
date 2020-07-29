#define DEBUG if(1)
#include "gtest/gtest.h"

#include <iostream>
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/util/log.h"
#include "lazyparam_prover/memory/stack.h"

using namespace util;
using namespace tableau;

TEST(congruence_axioms,all) {
  StreamLogger _(std::cerr);
  memory::Alloc A;
  u64 pred = 1;
  Term v0(Var(A,0));
  Term v1(Var(A,1));
  // TODO: this is a change detector,
  // ideally test should check equivalence of got and want
  auto want = AndClause::make(A,
    Atom::eq(A,true,v0,v1),
    Atom(A,true,pred,{v0}),
    Atom(A,false,pred,{v1})
  );
  auto got = neg_cong_pred_axiom(A,pred,1).derived();
  if(got!=want) FAIL() << fmt("cong_pred_axiom() = %, want %",show(got),show(want));
}
