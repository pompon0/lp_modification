#ifndef REWRITE_TEST_UTILS_H_
#define REWRITE_TEST_UTILS_H_

#include "gtest/gtest.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/util/log.h"
#include <random>

namespace tableau {

// rel[i][j] = cmp(terms[i],terms[j]), where all elements of terms are valuated.
// assumes correctness of operator==(Term,Term).
template<typename Cmp> void expect_order(memory::Alloc &A, Cmp &cmp, vec<Term> terms, bool total) {
  size_t n = terms.size();
  vec<vec<OrderAtom::Relation>> rel(terms.size(),vec<OrderAtom::Relation>(n));
  for(size_t i=n; i--;) for(size_t j=n; j--;) rel[i][j] = cmp.cmp(terms[i],terms[j]);
  for(auto &t : terms) t = cmp.eval(A,t);

  // symmetry & irreflexivity
  for(size_t i=0; i<n; i++) for(size_t j=0; j<n; j++) {
    if(terms[i]==terms[j]){
      if(rel[i][j]==OrderAtom::E && rel[j][i]==OrderAtom::E) continue;
    } else {
      if(rel[i][j]==OrderAtom::U && rel[j][i]==OrderAtom::U && !total) continue;
      if(rel[i][j]==OrderAtom::L && rel[j][i]==OrderAtom::G) continue;
      if(rel[i][j]==OrderAtom::G && rel[j][i]==OrderAtom::L) continue;
    }
    ADD_FAILURE() << util::fmt("a = %, b = %, cmp(a,b) = %, cmp(b,a) = %",show(terms[i]),show(terms[j]),rel[i][j],rel[j][i]);
  }
  // transitivity
  for(size_t i=0; i<n; i++) for(size_t j=0; j<n; j++) for(size_t k=0; k<n; k++) {
    bool less = false;
    less |= rel[i][j]==OrderAtom::L && rel[j][k]==OrderAtom::L;
    less |= rel[i][j]==OrderAtom::E && rel[j][k]==OrderAtom::L;
    less |= rel[i][j]==OrderAtom::L && rel[j][k]==OrderAtom::E;
    if(!less) continue;
    EXPECT_EQ(OrderAtom::L,rel[i][k]) << util::fmt("% <= % <= %",show(terms[i]),show(terms[j]),show(terms[k]));
  }
}

template<typename Cmp> void expect_subterm_order(memory::Alloc &A, Cmp &cmp, Term t, Term subeq) {
  subeq = cmp.shallow_eval(subeq);
  if(subeq.type()!=Term::FUN) return;
  Fun f(subeq);
  for(size_t i=f.arg_count(); i--;) {
    EXPECT_EQ(OrderAtom::L,cmp.cmp(f.arg(i),t))
      << util::fmt("cmp(%,%) = %",show(cmp.eval(A,f.arg(i))),show(cmp.eval(A,t)));
    expect_subterm_order(A,cmp,t,f.arg(i));
  }
}

struct TestCtx {
  TestCtx(const TestCtx &) = delete;
  explicit TestCtx(uint64_t seed) : rnd(seed) { new_fun(0); }
  memory::Alloc A;
  std::minstd_rand rnd;
  vec<size_t> zero_arity_funs;
  vec<size_t> arity;
  size_t new_fun(size_t a){
    size_t f = arity.size();
    arity.push_back(a);
    if(a==0) zero_arity_funs.push_back(f);
    return f;
  }
  void new_random_funs(size_t n, size_t max_arity) { while(n--) new_fun(rnd()%(max_arity+1)); }
  template<typename ...T> std::function<Term(T...)> new_fun(){
    size_t f = new_fun(sizeof...(T));
    return [this,f](T ...args){ return Term(Fun(A,f,{args...})); };
  }
};

template<typename V> Term make_term(TestCtx &ctx, const V &val, bool ground, size_t max_depth) { FRAME("make_term");
  if(max_depth==0) {
    if(ground || ctx.rnd()%2) {
      auto &fs = ctx.zero_arity_funs;
      return Term(Fun(ctx.A,fs[ctx.rnd()%fs.size()],{}));
    } else {
      return Term(Var(ctx.A,ctx.rnd()%val.size()));
    }
  } else {
    auto f = ctx.rnd()%ctx.arity.size();
    Fun::Builder b(ctx.A,f,ctx.arity[f]);
    for(size_t i=0; i<ctx.arity[f]; i++) b.set_arg(i,make_term(ctx,val,ground,max_depth-1));
    return Term(b.build());
  }
}

template<typename V> void random_valuate(TestCtx &ctx, V &val, size_t max_depth, bool ground) { FRAME("random_valuate");
  for(size_t i=0; i<val.size(); i++) {
    auto x = Term(Var(ctx.A,i));
    if(val.eval(ctx.A,x).type()==Term::VAR) {
      if(!ground && ctx.rnd()%2==0) continue;
      if(!val.unify(ctx.A,x,make_term(ctx,val,true,max_depth))) {
        error("val.mgu() failed");
      }
    }
  }
}

template<typename Cmp> Term replace_smaller_subterm(TestCtx &ctx, Cmp &cmp, Term t, Term sub) { FRAME("replace_smaller_subterm");
  if(ctx.rnd()%3==0 && cmp.cmp(t,sub)==OrderAtom::L) return sub;
  if(t.type()!=Term::FUN) return t;
  Fun f(t);
  if(f.arg_count()==0) return t;
  Fun::Builder b(ctx.A,f.fun(),f.arg_count());
  for(size_t i=f.arg_count(); i--;) b.set_arg(i,f.arg(i));
  size_t i = ctx.rnd()%f.arg_count();
  b.set_arg(i,replace_smaller_subterm(ctx,cmp,f.arg(i),sub));
  return Term(b.build());
}

////////////////////////////////////////////

template<typename Ord> void ordering_test() {
  SCOPED_TRACE("ordering_test");
  TestCtx ctx(90830845);
  for(size_t arity=4; arity--;) ctx.new_fun(arity);
  for(size_t depth=0; depth<4; depth++) {
    ConstrainedValuation<Ord> ord; for(size_t i=4; i--;) ord.allocate(Var(ctx.A,0));
    random_valuate(ctx,ord,depth,false);
    vec<Term> T;
    // generate bunch of terms: both ground and non-ground
    for(size_t n=10; n--;) T.push_back(make_term(ctx,ord,false,depth)); 
    for(size_t n=10; n--;) T.push_back(make_term(ctx,ord,true,depth)); 
    expect_order(ctx.A,ord,T,false);
  }
}

template<typename Ord> void ground_total_ordering_test() {
  SCOPED_TRACE("ground_total_ordering_test");
  TestCtx ctx(87539745);
  for(size_t arity=4; arity--;) ctx.new_fun(arity);
  for(size_t depth=0; depth<4; depth++) {
    ConstrainedValuation<Ord> ord; for(size_t i=3; i--;) ord.allocate(Var(ctx.A,0));
    random_valuate(ctx,ord,depth,true);
    vec<Term> T;
    // generate bunch of ground terms
    for(size_t n=20; n--;) T.push_back(make_term(ctx,ord,true,depth)); 
    expect_order(ctx.A,ord,T,true);
  }
}

template<typename Ord> void subterm_test() {
  SCOPED_TRACE("subterm_test");
  TestCtx ctx(19843054);
  for(size_t arity=1;arity<4;arity++) ctx.new_fun(arity);
  for(size_t cases=10; cases--;) {
    ConstrainedValuation<Ord> ord; for(size_t i=10; i--;) ord.allocate(Var(ctx.A,0));
    random_valuate(ctx,ord,6,false);
    auto t = make_term(ctx,ord,false,4);
    expect_subterm_order(ctx.A,ord,t,t);
  }
}

template<typename Ord> void substitution_test() {
  SCOPED_TRACE("substitution_test");
  TestCtx ctx(1893443);
  for(size_t arity=1;arity<4;arity++) ctx.new_fun(arity);
  ConstrainedValuation<Ord> ord; for(size_t i=15; i--;) ord.allocate(Var(ctx.A,0));
  random_valuate(ctx,ord,6,false);
  for(size_t cases=20;cases;) {
    auto s = make_term(ctx,ord,false,5);
    auto t = make_term(ctx,ord,false,5);
    auto rel = ord.cmp(s,t);
    switch(rel) {
    case OrderAtom::E: continue;
    case OrderAtom::U: continue;
    case OrderAtom::L: continue;
    case OrderAtom::G: std::swap(s,t); break;
    default: error("cmp() = %",rel);
    }
    auto es = ord.eval(ctx.A,s);
    auto et = ord.eval(ctx.A,t);
    if(t==es && t==et) continue;
    cases--;
    EXPECT_EQ(OrderAtom::L,ord.cmp(es,et)) << util::fmt("cmp(%,%) = %",show(es),show(et));
  }
}

template<typename Ord> void monotonicity_test() {
  SCOPED_TRACE("monotonicity_test");
  TestCtx ctx(7895374);
  for(size_t arity=1;arity<4;arity++) ctx.new_fun(arity);
  ConstrainedValuation<Ord> ord; for(size_t i=5; i--;) ord.allocate(Var(ctx.A,0));
  random_valuate(ctx,ord,3,false);
  for(size_t cases=20;cases;) {
    auto t = make_term(ctx,ord,false,6);
    auto s = make_term(ctx,ord,false,3);
    auto t2 = replace_smaller_subterm(ctx,ord,t,s);
    if(t==t2) continue;
    cases--;
    EXPECT_EQ(OrderAtom::L,ord.cmp(t,t2)) << util::fmt("cmp(%,%)",show(t),show(t2));
  }
}
template<typename Ord> void reduction_ordering_test_suite() {
  ordering_test<Ord>();
  ground_total_ordering_test<Ord>();
  subterm_test<Ord>();
  substitution_test<Ord>();
  monotonicity_test<Ord>();
}

}  // namespace tableau

#endif  // REWRITE_TEST_UTILS_H_
