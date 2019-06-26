#define DEBUG if(1)
#include "gtest/gtest.h"

#include <iostream>
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/util/log.h"

using namespace util;
TEST(congruence_axioms,all) {
  StreamLogger _(std::cerr);
  u64 pred = 1;
  OrClause want(2);
  Term v0(Var::make(0));
  Term v1(Var::make(1));
  // TODO: this is a change detector,
  // ideally test should check equivalence of got and want
  Atom::Builder b0(false,pred,1,0); b0.set_arg(0,v0);
  Atom::Builder b1(true,pred,1,0); b1.set_arg(0,v1);
  want.atoms = { Atom::eq(false,v0,v1), b0.build(), b1.build() };
  
  OrClause got = cong_pred_axiom(pred,1);
  if(got!=want) FAIL() << fmt("cong_pred_axiom() = %, want %",show(got),show(want));
}
