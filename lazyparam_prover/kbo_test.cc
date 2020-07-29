#define DEBUG_MODE
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/rewrite_test_utils.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "gtest/gtest.h"

using namespace tableau;

TEST(KBO,reduction_ordering) {
  StreamLogger _(std::cerr);
  reduction_ordering_test_suite<KBO>();
}

TEST(KBO,simple) {
  StreamLogger _(std::cerr);
  memory::Alloc A;
  ConstrainedValuation<KBO> kbo(A);
  Term x(kbo.allocate(Var(A,0)));
  Term y(kbo.allocate(Var(A,0)));
  auto f = [&A](auto ...a){ return Term(Fun(A,0,{a...})); };
  auto g = [&A](auto ...a){ return Term(Fun(A,1,{a...})); };
  auto h = [&A](auto ...a){ return Term(Fun(A,2,{a...})); };

  ASSERT_EQ(OrderAtom::E,kbo.cmp(x,x));
  ASSERT_EQ(OrderAtom::L,kbo.cmp(x,f(x)));
  ASSERT_EQ(OrderAtom::G,kbo.cmp(f(x),x));
  ASSERT_EQ(OrderAtom::U,kbo.cmp(x,f(y)));
  ASSERT_EQ(OrderAtom::L,kbo.cmp(x,f(y,x)));
  ASSERT_EQ(OrderAtom::L,kbo.cmp(g(x),f(y,g(x))));
  ASSERT_EQ(OrderAtom::L,kbo.cmp(g(g(x)),f(y,g(x))));
  ASSERT_EQ(OrderAtom::U,kbo.cmp(g(g(g(g(y)))),f(y,g(x))));
  ASSERT_EQ(OrderAtom::U,kbo.cmp(f(y,g(x)),g(g(g(g(y))))));
  ASSERT_EQ(OrderAtom::L,kbo.cmp(f(y,g(x)),f(y,h(x))));
}
