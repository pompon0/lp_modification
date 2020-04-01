#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/util/log.h"

using namespace tableau;

TEST(KBO,simple) {
  StreamLogger _(std::cerr);
  KBO kbo;
  kbo.resize(2);
  Term x(Var(0));
  Term y(Var(1));
  auto f = [](auto ...a){ return Term(Fun(0,{a...})); };
  auto g = [](auto ...a){ return Term(Fun(1,{a...})); };
  auto h = [](auto ...a){ return Term(Fun(2,{a...})); };

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
