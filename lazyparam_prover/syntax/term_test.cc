#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/util/log.h"

using namespace tableau;

TEST(Term,var_range) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  Term x(Var(S,0));
  Term y(Var(S,1));
  auto f = [&S](auto ...a){ return Term(Fun(S,0,{a...})); };
  auto g = [&S](auto ...a){ return Term(Fun(S,1,{a...})); };
  ASSERT_EQ(x.var_range(),(VarRange{0,1}));
  ASSERT_EQ(y.var_range(),(VarRange{1,2}));
  ASSERT_EQ(f(x).var_range(),(VarRange{0,1}));
  ASSERT_EQ(f(y,x).var_range(),(VarRange{0,2}));
  ASSERT_EQ(g(x).var_range(),(VarRange{0,1}));
  ASSERT_EQ(f(y,g(x)).var_range(),(VarRange{0,2}));
}
