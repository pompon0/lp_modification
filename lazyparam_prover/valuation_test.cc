#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/valuation.h"
#include "lazyparam_prover/memory/stack.h"
#include "utils/log.h"

using namespace tableau;

TEST(MGU,flat_loop) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  Valuation V;
  Term var0(V.allocate(Var(S,0)));
  Term var1(V.allocate(Var(S,0)));
  ASSERT_TRUE(V.unify(var1,var0));
  ASSERT_TRUE(V.unify(var0,var1));
  ASSERT_TRUE(!V[0]);
  ASSERT_EQ(V[1].get(),var0);
}

TEST(MGU,nonflat_loop) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  Valuation V;
  Term v0(V.allocate(Var(S,0)));
  Term v1(V.allocate(Var(S,0)));
  u64 f = 0;
  ASSERT_TRUE(V.unify(v0,Term(Fun(S,f,{v1}))));
  ASSERT_FALSE(V.unify(v1,Term(Fun(S,f,{v0}))));
}

TEST(MGU,equal) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  Valuation V;
  u64 f = 0;
  Term x(V.allocate(Var(S,0)));
  Term y(V.allocate(Var(S,0)));
  Term fx(Fun(S,f,{x}));
  Term ffx(Fun(S,f,{fx}));
  Term fy(Fun(S,f,{y}));
  ASSERT_TRUE(V.unify(y,fx));
  ASSERT_TRUE(V.equal(x,x));
  ASSERT_TRUE(V.equal(y,y));
  ASSERT_TRUE(V.equal(y,fx));
  ASSERT_TRUE(V.equal(fy,ffx));
  ASSERT_FALSE(V.equal(y,x));
  ASSERT_FALSE(V.equal(fy,x));
  ASSERT_FALSE(V.equal(y,ffx));
}

TEST(MGU,free_vars_size) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  Valuation V;
  Term x(V.allocate(Var(S,0)));
  Term y(V.allocate(Var(S,0)));
  Term z(V.allocate(Var(S,0)));
  ASSERT_EQ(3,V.free_vars_size());
  u64 f = 0;
  Term fx(Fun(S,f,{x}));
  ASSERT_TRUE(V.unify(y,fx));
  ASSERT_EQ(2,V.free_vars_size());
  ASSERT_TRUE(V.unify(x,z));
  ASSERT_EQ(1,V.free_vars_size());
}

TEST(MGU,eval) {
  StreamLogger _(std::cerr);
  memory::Alloc S;
  Valuation V;
  Term c(Fun(S,0,{}));
  Term x(V.allocate(Var(S,0)));
  ASSERT_TRUE(V.unify(c,x));

  Atom a(S,false,5,{c,x});
  Atom b(S,false,5,{c,c});
  auto ca = AndClause::make(S,a,b,a);
  auto cb = AndClause::make(S,b,b,b);
  DerAndClause::Builder builder(S);
  builder.derived = ca;
  builder.sources = {ca,ca,cb};
  builder.constraints = {OrderAtom::neq(S,a,b)};
  auto before = builder.build(S);
  builder.derived = cb;
  builder.sources = {cb,cb,cb};
  builder.constraints = {OrderAtom::neq(S,b,b)};
  auto got = V.eval(S,before);
  auto want = builder.build(S);
  ASSERT_FALSE(before==got);
  ASSERT_EQ(got,want);
}
