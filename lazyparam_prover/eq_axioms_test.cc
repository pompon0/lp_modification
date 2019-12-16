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
  size_t var_count = 0;
  Term v0(Var::make(var_count++));
  Term v1(Var::make(var_count++));
  OrClause::Builder wantB(3,var_count);
  // TODO: this is a change detector,
  // ideally test should check equivalence of got and want
  Atom::Builder b0(false,pred,1); b0.set_arg(0,v0);
  Atom::Builder b1(true,pred,1); b1.set_arg(0,v1);
  wantB.set_atom(0,Atom::eq(false,v0,v1));
  wantB.set_atom(1,b0.build());
  wantB.set_atom(2,b1.build());
  auto want = wantB.build();
  
  OrClause got = cong_pred_axiom(pred,1).derived();
  if(got!=want) FAIL() << fmt("cong_pred_axiom() = %, want %",show(got),show(want));
}
