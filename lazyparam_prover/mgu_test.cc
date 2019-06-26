#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/util/log.h"

TEST(MGU,loop) {
  Valuation V;
  auto var0 = Term(Var::make(0));
  auto var1 = Term(Var::make(1));
  OrClause cla(2); cla.atoms.push_back(Atom::eq(1,var0,var1));
  cla = V.alloc_vars(cla);
  ASSERT_TRUE(V.mgu(var1,var0));
  ASSERT_TRUE(V.mgu(var0,var1));
  ASSERT_TRUE(!V.val[0]);
  ASSERT_EQ(V.val[1].get(),var0);
}

