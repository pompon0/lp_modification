#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/util/log.h"

using namespace tableau;

TEST(Term,var_range) {
  StreamLogger _(std::cerr);
  Term x(Var(0));
  Term y(Var(1));
  auto f = [](auto ...a){ return Term(Fun(0,{a...})); };
  auto g = [](auto ...a){ return Term(Fun(1,{a...})); };
  ASSERT_EQ(x.var_range(),(VarRange{0,1}));
  ASSERT_EQ(y.var_range(),(VarRange{1,2}));
  ASSERT_EQ(f(x).var_range(),(VarRange{0,1}));
  ASSERT_EQ(f(y,x).var_range(),(VarRange{0,2}));
  ASSERT_EQ(g(x).var_range(),(VarRange{0,1}));
  ASSERT_EQ(f(y,g(x)).var_range(),(VarRange{0,2}));
}
