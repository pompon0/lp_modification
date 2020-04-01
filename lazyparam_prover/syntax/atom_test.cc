#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/util/string.h"

using namespace tableau;

TEST(pred,neg) {
  bool sign = true;
  size_t pred_name = 7;
  Atom::Builder b(sign,pred_name,2);
  b.set_arg(0,Term(Var(0)));
  b.set_arg(1,Term(Var(4)));
  Atom a = b.build();
  ASSERT_EQ(a.sign(),sign);
  ASSERT_EQ(a.pred(),pred_name);
  ASSERT_NE(a.neg(),a) << util::fmt("expected % != %",show(a.neg()),show(a));
  ASSERT_EQ(a.neg().neg(),a) << util::fmt("got %, want %",show(a.neg().neg()),show(a));
}
