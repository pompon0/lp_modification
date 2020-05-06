#define DEBUG
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/log.h"
#include "gtest/gtest.h"

using namespace tableau;

TEST(DerAndClause,shift) {
  StreamLogger _(std::cerr);
  Term x(Var(0));
  Term y(Var(1));
  DerAndClause::Builder b;
  b.derived = AndClause({Atom::eq(false,x,y)});
  b.sources.push_back(AndClause({Atom(true,4,{x,x,y})}));
  b.constraints.push_back(OrderAtom(OrderAtom::L,x,y));
  auto cla = b.build();
  EXPECT_EQ(cla.shift(4).derived(),cla.derived().shift(4));
  EXPECT_EQ(cla.shift(10).source(0),cla.source(0).shift(10));
  EXPECT_EQ(cla.shift(34).constraint(0),cla.constraint(0).shift(34));
}
