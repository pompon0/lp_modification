#define DEBUG if(1)
#include "gtest/gtest.h"

#include <iostream>
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/util/log.h"

using namespace util;
using namespace tableau;

TEST(congruence_axioms,all) {
  StreamLogger _(std::cerr);
  u64 pred = 1;
  Term v0(Var(0));
  Term v1(Var(1));
  // TODO: this is a change detector,
  // ideally test should check equivalence of got and want
  OrClause want({
    Atom::eq(false,v0,v1),
    Atom(false,pred,{v0}),
    Atom(true,pred,{v1}),
  });
  OrClause got = cong_pred_axiom(pred,1).derived();
  if(got!=want) FAIL() << fmt("cong_pred_axiom() = %, want %",show(got),show(want));
}
