#define DEBUG_MODE
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/log.h"
#include "gtest/gtest.h"

using namespace tableau;

TEST(OrderAtom,shift) {
  StreamLogger _(std::cerr);
  memory::Alloc A;
  Term x(Var(A,0));
  Term y(Var(A,1));
  OrderAtom a(A,OrderAtom::L,x,y);
  EXPECT_EQ(a.shift(4).pair(0).a,x.shift(4));
  EXPECT_EQ(a.shift(10).pair(0).b,y.shift(10));
}
