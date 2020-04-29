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
inline AndClause neg_trans_axiom(Term a, Term b, Term c) {
  return AndClause({
    Atom::eq(true,a,b),
    Atom::eq(true,b,c),
    Atom::eq(false,a,c),
  });
}

// a = b /\ b != a
inline AndClause neg_symm_axiom(Term a, Term b) {
  return AndClause({
    Atom::eq(true,a,b),
    Atom::eq(false,b,a),
  });
}

// a != a
inline AndClause neg_refl_axiom(Term a) {
  return AndClause({Atom::eq(false,a,a)});
}

inline Atom red(bool sign, Term f, Term w) {
  Atom::Builder b(sign,Atom::EQ_TRANS_POS,2,false);
  b.set_arg(0,f);
  b.set_arg(1,w);
  return b.build();
}

struct VarMap {
  vec<size_t> V;
  size_t n;
  
  VarMap(const DerAndClause &cla) : V(cla.var_range().end,0), n(0) {
    count(cla.derived());
    for(size_t i=cla.source_count(); i--;) count(cla.source(i)); 
    for(size_t i=cla.constraint_count(); i--;) count(cla.constraint(i)); 
    for(auto &i : V) if(i!=0) i = n++;
  }
  void count(AndClause cla) { for(size_t i=cla.atom_count(); i--;) count(cla.atom(i)); }
  void count(Atom a) { for(size_t i=a.arg_count(); i--;) count(a.arg(i)); }
  void count(OrderAtom c){ for(size_t i=c.pair_count(); i--;) count(c.pair(i)); }
  void count(OrderAtom::TermPair p) { count(p.a); count(p.b); }
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
    Atom::Builder b(a.sign(),a.pred(),a.arg_count(),a.strong_only());
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

DerAndClause reduce_vars(DerAndClause cla) {
  VarMap M(cla);
  auto b = cla.to_builder();
  b.derived = M.map(b.derived);
  for(auto &s : b.sources) s = M.map(s);
  for(auto &c : b.constraints) c = M.map(c);
  return b.build();
}

struct SplitBuilder {
  size_t cost;
  vec<Atom> atoms;
  vec<AndClause> source;
  vec<OrderAtom> constraints;
  DerAndClause out() { FRAME("out");
    DerAndClause::Builder b;
    b.cost = cost;
    b.derived = AndClause(atoms);
    b.sources = source;
    b.constraints = constraints;
    return reduce_vars(val.eval(b.build()));
  }

  vec<DerAndClause> extra;
  Valuation val;
  u64 next_pred;

  SplitBuilder(DerAndClause cla, u64 _next_pred) { FRAME("SplitBuilder");
    next_pred = _next_pred;
    cla = val.allocate(cla);
    cost = cla.cost();
    for(size_t i=0; i<cla.source_count(); ++i) source.push_back(cla.source(i));
    DEBUG if(cla.constraint_count()>0) error("unexpected constraints");
    for(size_t i=0; i<cla.derived().atom_count(); ++i) { FRAME("processing %",show(cla.derived().atom(i)));
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
        source.push_back(a.sign() ? neg_symm_axiom(l,r) : neg_symm_axiom(r,l));
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
          Term w(val.allocate(Var(0)));
          atoms.push_back(red(true,l,w));
          atoms.push_back(red(true,r,w));
          constraints.push_back(OrderAtom(OrderAtom::LE,w,l));
          constraints.push_back(OrderAtom(OrderAtom::LE,w,r));
          source.push_back(neg_trans_axiom(l,w,r));
          source.push_back(neg_symm_axiom(r,w));
        }
      } else {
        if(l.type()==Term::VAR) {
          // x!=y /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ x-/>y  [x!=y]
          // ==> T(x,y) /\ y-/>x  [y!=x]
          Atom a(false,next_pred++,{l,r});
          a = a.set_strong_only();
          atoms.push_back(a);

          {
            DerAndClause::Builder c1;
            c1.cost = 1;
            c1.derived = AndClause({a.neg(),red(false,l,r)});
            c1.constraints.push_back(OrderAtom(OrderAtom::NE,l,r));
            extra.push_back(reduce_vars(c1.build()));
          } {
            DerAndClause::Builder c2;
            c2.cost = 1;
            c2.derived = AndClause({a.neg(),red(false,r,l)});
            c2.constraints = {OrderAtom(OrderAtom::NE,r,l)};
            c2.sources.push_back(neg_symm_axiom(l,r));
            extra.push_back(reduce_vars(c2.build()));
          }
        } else if(r.type()==Term::VAR) {
          // f(x)!=y /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ f(x)-/>y  [f(x)!=y]
          // ==> T(x,y) /\ y-/>w /\ f(x)->w  [y!=w /\ f(x)>=w]
          Fun lf(l);
          size_t lc = lf.arg_count();
          Atom::Builder b(false,next_pred++,lc+1,true);
          for(size_t i=lc; i--;) b.set_arg(i,lf.arg(i));
          b.set_arg(lc,r);
          auto a = b.build();
          atoms.push_back(a);

          {
            DerAndClause::Builder c1;
            c1.cost = 1;
            c1.derived = AndClause({a.neg(),red(false,l,r)});
            c1.constraints = {OrderAtom(OrderAtom::NE,l,r)};
            extra.push_back(reduce_vars(c1.build()));
          }
          {
            DerAndClause::Builder c2;
            c2.cost = 1;
            Term w(val.allocate(Var(0)));
            c2.derived = AndClause({a.neg(),red(false,r,w),red(true,l,w)});
            c2.constraints = {
              OrderAtom(OrderAtom::NE,r,w),
              OrderAtom(OrderAtom::LE,w,l),
            };
            c2.sources = {
              neg_trans_axiom(r,l,w),
              neg_symm_axiom(l,r),
            };
            extra.push_back(reduce_vars(c2.build()));
          }
        } else {
          // f(x)!=g(y) /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ f(x)-/>w /\ g(y)->w  [f(x)!=w /\ g(y)>=w]
          // ==> T(x,y) /\ g(y)-/>w /\ f(x)->w  [g(y)!=w /\ f(x)>=w]
          Fun lf(l), rf(r);
          size_t lc = lf.arg_count(), rc = rf.arg_count();
          Atom::Builder b(false,next_pred++,lc+rc,true);
          for(size_t i=0; i<lc; ++i) b.set_arg(i,lf.arg(i));
          for(size_t i=0; i<rc; ++i) b.set_arg(lc+i,rf.arg(i));
          auto a = b.build();
          atoms.push_back(a);

          { FRAME("T(x,y) /\\ f(x)-/>w /\\ g(y)->w");
            DerAndClause::Builder c1;
            c1.cost = 1;
            Term w(val.allocate(Var(0)));
            c1.derived = AndClause({a.neg(),red(false,l,w),red(true,r,w)});
            c1.constraints = {
              OrderAtom(OrderAtom::NE,l,w),
              OrderAtom(OrderAtom::LE,w,r),
            };
            c1.sources = {neg_trans_axiom(l,r,w)};
            extra.push_back(reduce_vars(c1.build()));
          } { FRAME("T(x,y) /\\ g(y)-/>w /\\ f(x)->w"); 
            DerAndClause::Builder c2;
            c2.cost = 1;
            Term w(val.allocate(Var(0)));
            c2.derived = AndClause({a.neg(),red(true,l,w),red(false,r,w)});
            c2.constraints = {
              OrderAtom(OrderAtom::NE,r,w),
              OrderAtom(OrderAtom::LE,w,l),
            };
            c2.sources = {
              neg_trans_axiom(r,l,w),
              neg_symm_axiom(l,r),
            };
            extra.push_back(reduce_vars(c2.build()));
          }
        }
      }
    }
  }
};

OrForm conv(OrForm f) { FRAME("lazy::conv");
  //info("before =\n%\n",show(f));
  ArityCtx ac; ac.traverse(f);
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
  DerAndClause::Builder refl;
  refl.cost = 0;
  Term x(Var(0));
  refl.derived = AndClause({red(false,x,x)});
  refl.sources = {neg_refl_axiom(x)};
  f2.and_clauses.push_back(refl.build());
  //info("after =\n%\n",show(f2));
  return f2; 
}

} // namespace lazy
} // namespace tableau

#endif  // LAZY_H_
