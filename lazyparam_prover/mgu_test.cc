#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/util/log.h"

TEST(MGU,flat_loop) {
  auto var0 = Term(Var::make(0));
  auto var1 = Term(Var::make(1));
  OrClause cla(2); cla.atoms.push_back(Atom::eq(1,var0,var1));
  Valuation V(2);
  ASSERT_TRUE(V.mgu(var1,var0));
  ASSERT_TRUE(V.mgu(var0,var1));
  ASSERT_TRUE(!V.val[0]);
  ASSERT_EQ(V.val[1].get(),var0);
}

TEST(MGU,nonflat_loop) {
  Valuation V(2);
  Term v0(Var::make(0));
  Term v1(Var::make(1));
  u64 f = 0;
  ASSERT_TRUE(V.mgu(v0,Term(Fun::slow_make(f,{v1}))));
  ASSERT_FALSE(V.mgu(v1,Term(Fun::slow_make(f,{v0}))));
}
