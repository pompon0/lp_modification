#define DEBUG
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/log.h"
#include "gtest/gtest.h"

using namespace tableau;

TEST(OrderAtom,shift) {
  StreamLogger _(std::cerr);
  Term x(Var(0));
  Term y(Var(1));
  OrderAtom a(OrderAtom::L,x,y);
  EXPECT_EQ(a.shift(4).pair(0).a,x.shift(4));
  EXPECT_EQ(a.shift(10).pair(0).b,y.shift(10));
}
