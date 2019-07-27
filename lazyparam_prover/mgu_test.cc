#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/util/log.h"

TEST(MGU,flat_loop) {
  StreamLogger _(std::cerr);
  Term var0(Var::make(0));
  Term var1(Var::make(1));
  Valuation V; V.resize(2);
  ASSERT_TRUE(V.mgu(var1,var0));
  ASSERT_TRUE(V.mgu(var0,var1));
  ASSERT_TRUE(!V[0]);
  ASSERT_EQ(V[1].get(),var0);
}

TEST(MGU,nonflat_loop) {
  StreamLogger _(std::cerr);
  Valuation V; V.resize(2);
  Term v0(Var::make(0));
  Term v1(Var::make(1));
  u64 f = 0;
  ASSERT_TRUE(V.mgu(v0,Term(Fun::slow_make(f,{v1}))));
  ASSERT_FALSE(V.mgu(v1,Term(Fun::slow_make(f,{v0}))));
}