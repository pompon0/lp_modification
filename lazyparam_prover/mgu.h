#ifndef MGU_H_
#define MGU_H_

#include "utils/log.h"
#include "utils/string.h"
#include "utils/types.h"
#include "lazyparam_prover/memory/array.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/derived.h"

namespace tableau {

struct Valuation {
private:
  // there is NO cycles in valuation, even x -> x
  RewindArray<Term> val;
public:
  INL Valuation(){}
  INL Valuation(const Valuation &) = default;
  ~Valuation(){}
  using Save = RewindArray<Term>::Save;
  INL size_t size() const { return val.size(); }
  
  template<typename T> INL T allocate(T t) {
    VarRange r = t.var_range();
    t = t.shift(val.size()-r.begin);
    val.resize(val.size()+r.end-r.begin);
    return t;
  }

  INL Save save(){ return val.save(); }
  INL void restore(Save s){ 
    FRAME("Valuation::restore(): %",DebugString());
    val.restore(s);
  }
  INL Maybe<Term> operator[](size_t i){ return val[i]; }

  bool has_var(Term t, u64 v) const { FRAME("has_var(%,%)",show(t),v);
    switch(t.type()) {
      case Term::VAR: 
        if(auto mx = val[Var(t).id()]) return has_var(mx.get(),v);
        return Var(t).id()==v;
      case Term::FUN: {
        Fun f(t);
        for(auto i=f.arg_count(); i--;)
          if(has_var(f.arg(i),v)) return 1;
        return 0;
      }
    }
    error("has_var(<type=%>,v",t.type());
  }

  INL bool assign(u64 v, Term t) { FRAME("MGU.assign(%,%)",v,show(t));
    DEBUG if(val[v]) error("val[%] is already set",v);
    // traverse TVar assignments
    for(Maybe<Term> mtv; t.type()==Term::VAR && (mtv = val[Var(t).id()]); ) t = mtv.get();
    switch(t.type()) {
      case Term::VAR: {
        Var tv(t);
        // break on trivial assignment
        if(tv.id()==v) return 1;
        val.set(v,t);
        return 1;
      }
      case Term::FUN: {
        if(has_var(t,v)) return 0;
        val.set(v,t);
        return 1;
      }
    }
    error("Valuation::assign(v,<type=%>)",t.type());
    return 0;
  }

  bool equal(Term x, Term y) const {
    for(Maybe<Term> mxv; x.type()==Term::VAR && (mxv = val[Var(x).id()]);) x = mxv.get();
    for(Maybe<Term> myv; y.type()==Term::VAR && (myv = val[Var(y).id()]);) y = myv.get();
    if(x.type()!=y.type()) return 0;
    switch(x.type()) {
      case Term::VAR: return Var(x).id()==Var(y).id();
      case Term::FUN: {
        Fun fx(x),fy(y);
        if(fx.fun()!=fy.fun()) return 0;
        for(size_t i=fx.arg_count(); i--;) if(!equal(fx.arg(i),fy.arg(i))) return 0;
        return 1;
      }
    }
    error("equal(<type=%>,y)",x.type());
    return 0;
  }

  INL bool equal_mod_sign(Atom x, Atom y) {
    if(x.pred()!=y.pred()) return 0;
    for(size_t i=x.arg_count(); i--;) {
      if(!equal(x.arg(i),y.arg(i))) return 0;
    }
    return 1;
  }

  bool mgu(Term x, Term y) { FRAME("mgu(%,%) %",show(x),show(y),DebugString());
    // TODO: add this iff hash consing is implemented
    // if(t1==t2) return 1;
    if(x.type()==Term::FUN && y.type()==Term::FUN) {
      Fun xf(x), yf(y);
      if(xf.fun()!=yf.fun()) return 0;
      auto ac = xf.arg_count();
      for(size_t i=0; i<ac; ++i)
        if(!mgu(xf.arg(i),yf.arg(i))) return 0;
      return 1;
    }
    if(x.type()!=Term::VAR && y.type()==Term::VAR) swap(x,y);
    if(x.type()==Term::VAR) {
      Var xv(x);
      if(auto mx = val[xv.id()]) return mgu(mx.get(),y);
      SCOPE("Valuation::assign");
      return assign(xv.id(),y);
    }
    error("unhandled case (type %, type %)",x.type(),y.type());
    return 0;
  }

  // unifies atoms ignoring the sign
  INL bool mgu(Atom x, Atom y) { FRAME("opposite()");
    SCOPE("Valuation::mgu(Atom)");
    if(x.pred()!=y.pred()) return 0;
    DEBUG if(x.arg_count()!=y.arg_count()) error("arg_count() mismatch: %, %",show(x),show(y));
    auto s = save();
    for(size_t i=x.arg_count(); i--;)
      if(!mgu(x.arg(i),y.arg(i))){ restore(s); return 0; }
    return 1;
  }

  INL Term shallow_eval(Term t) const {
    while(t.type()==Term::VAR && val[Var(t).id()]) t = val[Var(t).id()].get();
    return t;
  }

  // clears offset
  Term eval(memory::Alloc &A, Term t) const { FRAME("eval(%)",show(t));
    switch(t.type()) {
      case Term::VAR: {
        u64 id = Var(t).id();
        if(auto mv = val[id]) return eval(A,mv.get()); else return Term(Var(A,id));
      }
      case Term::FUN: {
        Fun tf(t);
        size_t ac = tf.arg_count();
        Fun::Builder b(A,tf.fun(),ac);
        for(size_t i=0; i<ac; ++i) b.set_arg(i,eval(A,tf.arg(i)));
        return Term(b.build());
      }
      default: error("unhandled t.type() = %",t.type());
    }
  }

  // clears offset
  INL Atom eval(memory::Alloc &A, Atom a) const { FRAME("eval(%)",show(a));
    size_t ac = a.arg_count();
    Atom::Builder b(A,a.sign(),a.pred(),ac,a.strong_only());
    for(size_t i=ac; i--;) b.set_arg(i,eval(A,a.arg(i)));
    return b.build();
  }

  INL AndClause eval(memory::Alloc &A, AndClause cla) const { FRAME("eval(%)",show(cla));
    AndClause::Builder b(A,cla.atom_count());
    for(size_t i=cla.atom_count(); i--;) b.set_atom(i,eval(A,cla.atom(i)));
    return b.build();
  }

  INL OrderAtom::TermPair eval(memory::Alloc &A, OrderAtom::TermPair p) const {
    return {eval(A,p.a),eval(A,p.b)};
  }

  INL OrderAtom eval(memory::Alloc &A, OrderAtom c) const {
    OrderAtom::Builder b(A,c.rel(),c.pair_count());
    for(size_t i=c.pair_count(); i--;) b.set_pair(i,eval(A,c.pair(i)));
    return b.build();
  }

  INL DerAndClause eval(memory::Alloc &A, DerAndClause cla) const {
    auto b = cla.to_builder(A);
    b.derived = eval(A,b.derived);
    for(auto &s : b.sources) s = eval(A,s);
    for(auto &c : b.constraints) c = eval(A,c);
    return b.build(A);
  }

  str DebugString() const {
    vec<str> l;
    for(size_t i=0; i<val.size(); ++i) if(val[i]) {
      l.push_back(util::fmt("v% = %",i,show(val[i].get())));
    }
    return util::fmt("{ % }",util::join("; ",l));
  }
};

} // tableau

#endif // MGU_H_
