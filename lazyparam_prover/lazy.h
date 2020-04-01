#ifndef LAZY_H_
#define LAZY_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/mgu.h"

namespace tableau {
namespace lazy {

// a = b /\ b = c /\ a != c
inline AndClause trans_axiom(Term a, Term b, Term c) {
  return AndClause({
    Atom::eq(true,a,b),
    Atom::eq(true,b,c),
    Atom::eq(false,a,c),
  });
}

// a = b /\ b != a
inline AndClause symm_axiom(Term a, Term b) {
  return AndClause({
    Atom::eq(true,a,b),
    Atom::eq(false,b,a),
  });
}

inline AndClause refl_axiom(Term a) {
  return AndClause({Atom::eq(false,a,a)});
}

inline Atom red(bool sign, Term f, Term w) {
  Atom::Builder b(sign,Atom::EQ_TRANS_POS,2);
  b.set_arg(0,f);
  b.set_arg(1,w);
  return b.build();
}

struct VarMap {
  vec<size_t> V;
  size_t n;
  
  VarMap(const AndClause &cla) : V(cla.var_range().end,0), n(0) {
    for(size_t i=cla.atom_count(); i--;) count(cla.atom(i));
    for(auto &i : V) if(i!=0) i = n++;
  }
  void count(Atom a) { for(size_t i=a.arg_count(); i--;) count(a.arg(i)); }
  void count(Term t) {
    switch(t.type()) {
      case Term::VAR: V[Var(t).id()]++; break;
      case Term::FUN: {
        Fun f(t);
        for(size_t i=f.arg_count(); i--;) count(f.arg(i));
        break;
      }
    }
  }

  AndClause map(AndClause cla) {
    vec<Atom> x;
    for(size_t i=0; i<cla.atom_count(); ++i)
      x.push_back(map(cla.atom(i)));
    return AndClause(x);
  }
  Atom map(Atom a) {
    Atom::Builder b(a.sign(),a.pred(),a.arg_count());
    for(size_t i=a.arg_count(); i--;) b.set_arg(i,map(a.arg(i)));
    return b.build();
  }
  Term map(Term t) {
    switch(t.type()) {
      case Term::VAR: {
        Var v(t);
        DEBUG if(v.id()>=V.size()) error("unknown variable");
        return Term(Var(V[v.id()]));
      }
      case Term::FUN: {
        Fun f(t);
        Fun::Builder b(f.fun(),f.arg_count());
        for(size_t i=f.arg_count(); i--;) b.set_arg(i,map(f.arg(i)));
        return Term(b.build());
      }
      default: error("t.type() = %",t.type());
    }
  }
  OrderAtom::TermPair map(OrderAtom::TermPair p) { return {map(p.a),map(p.b)}; }
  OrderAtom map(OrderAtom c) {
    OrderAtom::Builder b(c.rel(),c.pair_count());
    for(size_t i=c.pair_count(); i--;) b.set_pair(i,map(c.pair(i)));
    return b.build();
  }
};

DerOrClause reduce_vars(DerOrClause cla) {
  VarMap M(cla.derived().neg());
  DerOrClause::Builder b(cla.source_count(),cla.constraint_count());
  b.set_cost(cla.cost());
  b.set_derived(M.map(cla.derived().neg()).neg());
  for(size_t i=cla.source_count(); i--;) b.set_source(i,M.map(cla.source(i).neg()).neg());
  for(size_t i=cla.constraint_count(); i--;) b.set_constraint(i,M.map(cla.constraint(i)));
  return b.build();
}

DerAndClause reduce_vars(DerAndClause cla) {
  return reduce_vars(cla.neg()).neg();
}

struct SplitBuilder {
  size_t cost;
  size_t var_count;
  vec<Atom> atoms;
  vec<AndClause> source;
  vec<OrderAtom> constraints;
  DerAndClause out() {
    DerOrClause::Builder b(source.size(),constraints.size());
    b.set_cost(cost);
    b.set_derived(AndClause(atoms).neg());
    for(size_t i=0; i<source.size(); ++i) b.set_source(i,source[i].neg());
    for(size_t i=0; i<constraints.size(); ++i) b.set_constraint(i,constraints[i]);
    return reduce_vars(val.eval(b.build().neg()));
  }

  vec<DerAndClause> extra;
  Valuation val;
  u64 next_pred;

  SplitBuilder(const DerAndClause &cla, u64 _next_pred) {
    next_pred = _next_pred;
    var_count = cla.var_range().end;
    val.resize(cla.var_range().end); // enough for substitutions
    cost = cla.cost();
    for(size_t i=0; i<cla.source_count(); ++i) source.push_back(cla.source(i));
    DEBUG if(cla.constraint_count()>0) error("unexpected constraints");
    for(size_t i=0; i<cla.derived().atom_count(); ++i) {
      auto a = cla.derived().atom(i);
      if(a.pred()!=Atom::EQ) {
        atoms.push_back(a);
        continue;
      }
      DEBUG if(a.arg_count()!=2) error("a.arg_count() = %",a.arg_count());
      auto l = a.arg(0);
      auto r = a.arg(1);
      if(l.type()==Term::VAR) {
        std::swap(l,r);
        source.push_back(a.sign() ? symm_axiom(l,r) : symm_axiom(r,l));
      }
      if(a.sign()) {
        if(l.type()==Term::VAR) {
          // x=y /\ C
          // ==> C[x=y]
          if(!val.mgu(l,r)) error("trivial unification failed");
        } else if(r.type()==Term::VAR) { 
          // f(x)=y /\ C
          // ==> f(x)->y /\ C  [f(x)>=y]
          atoms.push_back(red(true,l,r));
          constraints.push_back(OrderAtom(OrderAtom::LE,r,l));
        } else {
          // f(x)=g(y) /\ C
          // ==> f(x)->w /\ g(y)->w /\ C  [f(x)>=w /\ g(y)>=w]
          Term w(Var(var_count++));
          atoms.push_back(red(true,l,w));
          atoms.push_back(red(true,r,w));
          constraints.push_back(OrderAtom(OrderAtom::LE,w,l));
          constraints.push_back(OrderAtom(OrderAtom::LE,w,r));
          source.push_back(trans_axiom(l,w,r));
          source.push_back(symm_axiom(r,w));
        }
      } else {
        if(l.type()==Term::VAR) {
          // x!=y /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ x-/>y  [x!=y]
          // ==> T(x,y) /\ y-/>x  [y!=x]
          Atom a(false,next_pred++,{l,r});
          atoms.push_back(a);

          {
            DerOrClause::Builder c1(0,1);
            c1.set_cost(1);
            c1.set_derived(AndClause({
              a.neg(),
              red(false,l,r),
            }).neg());
            c1.set_constraint(0,OrderAtom(OrderAtom::NE,l,r));
            extra.push_back(reduce_vars(c1.build().neg()));
          } {
            DerOrClause::Builder c2(1,1);
            c2.set_cost(1);
            c2.set_derived(AndClause({
              a.neg(),
              red(false,r,l),
            }).neg());
            c2.set_constraint(0,OrderAtom(OrderAtom::NE,r,l));
            c2.set_source(0,symm_axiom(l,r).neg());
            extra.push_back(reduce_vars(c2.build().neg()));
          }
        } else if(r.type()==Term::VAR) {
          // f(x)!=y /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ f(x)-/>y  [f(x)!=y]
          // ==> T(x,y) /\ y-/>w /\ f(x)->w  [y!=w /\ f(x)>=w]
          Fun lf(l);
          size_t lc = lf.arg_count();
          Atom::Builder b(false,next_pred++,lc+1);
          for(size_t i=lc; i--;) b.set_arg(i,lf.arg(i));
          b.set_arg(lc,r);
          atoms.push_back(b.build());

          {
            DerOrClause::Builder c1(0,1);
            c1.set_cost(1);
            c1.set_derived(AndClause({a.neg(),red(false,l,r)}).neg());
            c1.set_constraint(0,OrderAtom(OrderAtom::NE,l,r));
            extra.push_back(reduce_vars(c1.build().neg()));
          }
          {
            DerOrClause::Builder c2(2,2);
            c2.set_cost(1);
            Term w(Var(var_count++));
            c2.set_derived(AndClause({a.neg(),red(false,r,w),red(true,l,w)}).neg());
            c2.set_constraint(0,OrderAtom(OrderAtom::NE,r,w));
            c2.set_constraint(1,OrderAtom(OrderAtom::LE,w,l));
            c2.set_source(0,trans_axiom(r,l,w).neg());
            c2.set_source(1,symm_axiom(l,r).neg());
            extra.push_back(reduce_vars(c2.build().neg()));
          }
        } else {
          // f(x)!=g(y) /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ f(x)-/>w /\ g(y)->w  [f(x)!=w /\ g(y)>=w]
          // ==> T(x,y) /\ g(y)-/>w /\ f(x)->w  [g(y)!=w /\ f(x)>=w]
          Fun lf(l), rf(r);
          size_t lc = lf.arg_count(), rc = rf.arg_count();
          Atom::Builder b(false,next_pred++,lc+rc);
          for(size_t i=0; i<lc; ++i) b.set_arg(i,lf.arg(i));
          for(size_t i=0; i<rc; ++i) b.set_arg(lc+i,rf.arg(i));
          atoms.push_back(b.build());

          {
            DerOrClause::Builder c1(1,2);
            c1.set_cost(1);
            Term w(Var(var_count++));
            c1.set_derived(AndClause({a.neg(),red(false,l,w),red(true,r,w)}).neg());
            c1.set_constraint(0,OrderAtom(OrderAtom::NE,l,w));
            c1.set_constraint(1,OrderAtom(OrderAtom::LE,w,r));
            c1.set_source(0,trans_axiom(l,r,w).neg());
            extra.push_back(reduce_vars(c1.build().neg()));
          } { 
            DerOrClause::Builder c2(2,2);
            c2.set_cost(1);
            Term w(Var(var_count++));
            c2.set_derived(AndClause({a.neg(),red(true,l,w),red(false,r,w)}).neg());
            c2.set_constraint(0,OrderAtom(OrderAtom::NE,r,w));
            c2.set_constraint(1,OrderAtom(OrderAtom::LE,w,l));
            c2.set_source(0,trans_axiom(r,l,w).neg());
            c2.set_source(1,symm_axiom(l,r).neg());
            extra.push_back(reduce_vars(c2.build().neg()));
          }
        }
      }
    }
    val.resize(var_count);
  }
};

OrForm conv(OrForm f) {
  //info("before =\n%\n",show(f));
  ArityCtx ac; ac.traverse(NotAndForm(f));
  f = flatten_OrForm(f);
  //info("flattened =\n%\n",show(f));
  OrForm f2;
  u64 next_pred = 0;
  for(auto &p : ac.pred_count) if(p.first<Atom::PRED_MIN && p.first>=next_pred) next_pred = p.first+1;
  for(auto &c : f.and_clauses) {
    SplitBuilder b(c,next_pred);
    f2.and_clauses.push_back(b.out());
    f2.and_clauses.insert(f2.and_clauses.end(),b.extra.begin(),b.extra.end());
    next_pred = b.next_pred;
  }
  // ==> x-/>x
  DerAndClause refl;
  refl.cost = 0;
  refl.derived.var_count = 0;
  Term x(Var::make(refl.derived.var_count++));
  refl.derived.atoms.push_back(red(false,x,x));
  refl.source.push_back(refl_axiom(x));
  f2.and_clauses.push_back(refl);
  //info("after =\n%\n",show(f2));
  return f2; 
}

} // namespace lazy
} // namespace tableau

#endif  // LAZY_H_
