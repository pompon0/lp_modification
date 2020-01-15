#ifndef LAZY_H_
#define LAZY_H_

#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/mgu.h"

namespace tableau {
namespace lazy {

// a = b /\ b = c /\ a != c
inline AndClause trans_axiom(Term a, Term b, Term c) {
  AndClause cla;
  cla.atoms = {
    Atom::eq(true,a,b),
    Atom::eq(true,b,c),
    Atom::eq(false,a,c)
  };
  return cla;
}

// a = b /\ b != a
inline AndClause symm_axiom(Term a, Term b) {
  AndClause cla;
  cla.atoms = {
    Atom::eq(true,a,b),
    Atom::eq(false,b,a)
  };
  return cla;
}

inline AndClause refl_axiom(Term a) {
  AndClause cla;
  cla.atoms = {Atom::eq(false,a,a)};
  return cla;
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
  
  VarMap(const AndClause &cla) : V(cla.var_count,0), n(0) {
    for(const auto &a : cla.atoms) count(a);
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
    for(auto &a : cla.atoms) a = map(a);
    cla.var_count = n;
    return cla;
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
        return Term(Var::make(V[v.id()]));
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
  Constraint map(Constraint c) {
    List<Constraint::Pair> or_;
    for(auto o = c.or_; !o.empty(); o = o.tail())
      or_ += Constraint::Pair{map(o.head().l),map(o.head().r)};
    c.or_ = or_;
    return c;
  }
};

DerAndClause reduce_vars(DerAndClause cla) {
  VarMap M(cla.derived);
  cla.derived = M.map(cla.derived);
  for(auto &s : cla.source) s = M.map(s);
  for(auto &c : cla.constraints) c = M.map(c);
  return cla;
}

struct SplitBuilder {
  DerAndClause out;
  vec<DerAndClause> extra;
  Valuation val;
  u64 next_pred;

  SplitBuilder(const DerAndClause &cla, u64 _next_pred) {
    next_pred = _next_pred;
    val.resize(cla.derived.var_count); // enough for substitutions
    out.cost = cla.cost;
    out.source = cla.source;
    out.derived.var_count = cla.derived.var_count;
    DEBUG if(cla.constraints.size()>0) error("unexpected constraints");
    for(auto a : cla.derived.atoms) {
      if(a.pred()!=Atom::EQ) {
        out.derived.atoms.push_back(a);
        continue;
      }
      DEBUG if(a.arg_count()!=2) error("a.arg_count() = %",a.arg_count());
      auto l = a.arg(0);
      auto r = a.arg(1);
      if(l.type()==Term::VAR) {
        std::swap(l,r);
        out.source.push_back(a.sign() ? symm_axiom(l,r) : symm_axiom(r,l));
      }
      if(a.sign()) {
        if(l.type()==Term::VAR) {
          // x=y /\ C
          // ==> C[x=y]
          if(!val.mgu(l,r)) error("trivial unification failed");
        } else if(r.type()==Term::VAR) { 
          // f(x)=y /\ C
          // ==> f(x)->y /\ C  [f(x)>=y]
          out.derived.atoms.push_back(red(true,l,r));
          //out.constraints.push_back(Constraint::le(r,l));
        } else {
          // f(x)=g(y) /\ C
          // ==> f(x)->w /\ g(y)->w /\ C  [f(x)>=w /\ g(y)>=w]
          Term w(Var::make(out.derived.var_count++));
          out.derived.atoms.push_back(red(true,l,w));
          out.derived.atoms.push_back(red(true,r,w));
          //out.constraints.push_back(Constraint::le(w,l));
          //out.constraints.push_back(Constraint::le(w,r));
          out.source.push_back(trans_axiom(l,w,r));
          out.source.push_back(symm_axiom(r,w));
        }
      } else {
        if(l.type()==Term::VAR) {
          // x!=y /\ C
          // ==> -T(x,y) /\ C
          // ==> T(x,y) /\ x-/>y  [x!=y]
          // ==> T(x,y) /\ y-/>x  [y!=x]
          Atom::Builder b(false,next_pred++,2);
          b.set_arg(0,l);
          b.set_arg(1,r);
          out.derived.atoms.push_back(b.build());

          {
            DerAndClause c1;
            c1.cost = 1;
            c1.derived.var_count = out.derived.var_count;
            c1.derived.atoms.push_back(b.build().neg());
            c1.derived.atoms.push_back(red(false,l,r));
            //c1.constraints.push_back(Constraint::neq(l,r));
            extra.push_back(reduce_vars(c1));
          } {
            DerAndClause c2;
            c2.cost = 1;
            c2.derived.var_count = out.derived.var_count;
            c2.derived.atoms.push_back(b.build().neg());
            c2.derived.atoms.push_back(red(false,r,l));
            //c2.constraints.push_back(Constraint::neq(r,l));
            c2.source.push_back(symm_axiom(l,r));
            extra.push_back(reduce_vars(c2));
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
          out.derived.atoms.push_back(b.build());

          {
            DerAndClause c1;
            c1.cost = 1;
            c1.derived.var_count = out.derived.var_count;
            c1.derived.atoms.push_back(b.build().neg());
            c1.derived.atoms.push_back(red(false,l,r));
            //c1.constraints.push_back(Constraint::neq(l,r));
            extra.push_back(reduce_vars(c1));
          }
          {
            DerAndClause c2;
            c2.cost = 1;
            c2.derived.var_count = out.derived.var_count;
            Term w(Var::make(c2.derived.var_count++));
            c2.derived.atoms.push_back(b.build().neg());
            c2.derived.atoms.push_back(red(false,r,w));
            c2.derived.atoms.push_back(red(true,l,w));
            //c2.constraints.push_back(Constraint::neq(r,w));
            //c2.constraints.push_back(Constraint::le(w,l));
            c2.source.push_back(trans_axiom(r,l,w));
            c2.source.push_back(symm_axiom(l,r));
            extra.push_back(reduce_vars(c2));
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
          out.derived.atoms.push_back(b.build());

          {
            DerAndClause c1;
            c1.cost = 1;
            c1.derived.var_count = out.derived.var_count;
            Term w(Var::make(c1.derived.var_count++));
            c1.derived.atoms.push_back(b.build().neg());
            c1.derived.atoms.push_back(red(false,l,w));
            c1.derived.atoms.push_back(red(true,r,w));
            //c1.constraints.push_back(Constraint::neq(l,w));
            //c1.constraints.push_back(Constraint::le(w,r));
            c1.source.push_back(trans_axiom(l,r,w));
            extra.push_back(reduce_vars(c1));
          } { 
            DerAndClause c2;
            c2.cost = 1;
            c2.derived.var_count = out.derived.var_count;
            Term w(Var::make(c2.derived.var_count++));
            c2.derived.atoms.push_back(b.build().neg());
            c2.derived.atoms.push_back(red(true,l,w));
            c2.derived.atoms.push_back(red(false,r,w));
            //c2.constraints.push_back(Constraint::neq(r,w));
            //c2.constraints.push_back(Constraint::le(w,l));
            c2.source.push_back(trans_axiom(r,l,w));
            c2.source.push_back(symm_axiom(l,r));
            extra.push_back(reduce_vars(c2));
          }
        }
      }
    }
    val.resize(out.derived.var_count);
    out = reduce_vars(val.eval(out));
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
    f2.and_clauses.push_back(b.out);
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
