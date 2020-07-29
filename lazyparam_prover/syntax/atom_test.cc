#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/memory/stack.h"

using namespace tableau;

TEST(pred,neg) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  bool sign = true;
  size_t pred_name = 7;
  Atom::Builder b(S,sign,pred_name,2,false);
  b.set_arg(0,Term(Var(S,0)));
  b.set_arg(1,Term(Var(S,4)));
  Atom a = b.build();
  ASSERT_EQ(a.sign(),sign);
  ASSERT_EQ(a.pred(),pred_name);
  ASSERT_NE(a.neg(),a) << util::fmt("expected % != %",show(a.neg()),show(a));
  ASSERT_EQ(a.neg().neg(),a) << util::fmt("got %, want %",show(a.neg().neg()),show(a));
}
