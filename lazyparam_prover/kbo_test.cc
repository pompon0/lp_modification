#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/util/log.h"

using namespace tableau;

TEST(KBO,simple) {
  StreamLogger _(std::cerr);
  KBO kbo;
  kbo.resize(2);
  Term x(Var::make(0));
  Term y(Var::make(1));
  auto f = [](auto ...a){ return Term(Fun::slow_make(0,{a...})); };
  auto g = [](auto ...a){ return Term(Fun::slow_make(1,{a...})); };
  auto h = [](auto ...a){ return Term(Fun::slow_make(2,{a...})); };

  ASSERT_EQ(KBO::E,kbo.cmp(x,x));
  ASSERT_EQ(KBO::L,kbo.cmp(x,f(x)));
  ASSERT_EQ(KBO::G,kbo.cmp(f(x),x));
  ASSERT_EQ(KBO::N,kbo.cmp(x,f(y)));
  ASSERT_EQ(KBO::L,kbo.cmp(x,f(y,x)));
  ASSERT_EQ(KBO::L,kbo.cmp(g(x),f(y,g(x))));
  ASSERT_EQ(KBO::L,kbo.cmp(g(g(x)),f(y,g(x))));
  ASSERT_EQ(KBO::N,kbo.cmp(g(g(g(g(y)))),f(y,g(x))));
  ASSERT_EQ(KBO::N,kbo.cmp(f(y,g(x)),g(g(g(g(y))))));
  ASSERT_EQ(KBO::L,kbo.cmp(f(y,g(x)),f(y,h(x))));
}
