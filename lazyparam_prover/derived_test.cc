#define DEBUG
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/syntax/term.h"
#include "utils/log.h"
#include "gtest/gtest.h"

using namespace tableau;

TEST(DerAndClause,shift) {
  StreamLogger _(std::cerr);
  memory::Alloc A;
  Term x(Var(A,0));
  Term y(Var(A,1));
  DerAndClause::Builder b(A);
  b.derived = AndClause::make(A,Atom::eq(A,false,x,y));
  b.sources.push_back(AndClause::make(A,Atom(A,true,4,{x,x,y})));
  b.constraints.push_back(OrderAtom(A,OrderAtom::L,x,y));
  auto cla = b.build(A);
  EXPECT_EQ(cla.shift(4).derived(),cla.derived().shift(4));
  EXPECT_EQ(cla.shift(10).source(0),cla.source(0).shift(10));
  EXPECT_EQ(cla.shift(34).constraint(0),cla.constraint(0).shift(34));
}
