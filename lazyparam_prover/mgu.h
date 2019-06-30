#ifndef MGU_H_
#define MGU_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/alloc.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/pred_format.h"
#include "lazyparam_prover/util/string.h"

struct Valuation {
  // there is NO cycles in valuation, even x -> x
  vec<Maybe<Term>> val;

  OrClause alloc_vars(OrClause cla) {
    size_t offset = val.size();
    val.resize(val.size()+cla.var_count);
    for(auto &a : cla.atoms) a = a.with_offset(offset);
    return cla;
  }

  inline bool has_var(Term t, u64 v) { FRAME("has_var(%,%)",show(t),v);
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

  inline bool assign(u64 v, Term t) { FRAME("MGU.assign(%,%)",v,show(t));
    DEBUG if(val[v]) error("val[%] is already set",v);
    switch(t.type()) {
      case Term::VAR: {
        Var tv(t);
        // break on trivial assignment
        if(tv.id()==v) return 1;
        // traverse TVar assignments
        if(auto mtv = val[tv.id()]) return assign(v,mtv.get());
        val[v] = t;
        return 1;
      }
      case Term::FUN: {
        if(has_var(t,v)) return 0;
        val[v] = t;
        return 1;
      }
    }
    error("MGU::assign(v,<type=%>)",t.type());
    return 0;
  }

  // unifies opposite atoms
  inline bool opposite(Atom x, Atom y) { FRAME("opposite()");
    if(x.sign()==y.sign()) return 0;
    if(x.pred()!=y.pred()) return 0;
    DEBUG if(x.arg_count()!=y.arg_count()) error("arg_count() mismatch: %, %",show(x),show(y));
    for(size_t i=x.arg_count(); i--;)
      if(!mgu(x.arg(i),y.arg(i))) return 0;
    return 1;
  }

  inline bool mgu(Term x, Term y) { FRAME("mgu(%,%) %",show(x),show(y),DebugString());
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
      return assign(xv.id(),y);
    }
    error("unhandled case (type %, type %)",x.type(),y.type());
    return 0;
  }

  // clears offset
  inline Term eval(Term t) { FRAME("eval(%)",show(t));
    switch(t.type()) {
      case Term::VAR: {
        u64 id = Var(t).id();
        if(auto mv = val[id]) return eval(mv.get()); else return Term(Var::make(id));
      }
      case Term::FUN: {
        Fun tf(t);
        size_t ac = tf.arg_count();
        Fun::Builder b(tf.fun(),ac);
        for(size_t i=0; i<ac; ++i) b.set_arg(i,eval(tf.arg(i)));
        return Term(b.build());
      }
      default: DEBUG error("unhandled t.type() = %",t.type());
    }
  }

  // clears offset
  inline Atom eval(Atom a) { FRAME("eval(%)",show(a));
    size_t ac = a.arg_count();
    Atom::Builder b(a.sign(),a.pred(),ac,0);
    for(size_t i=ac; i--;) b.set_arg(i,eval(a.arg(i)));
    return b.build();
  }

  // clears offset
  inline OrClause eval(OrClause cla) { FRAME("eval(%)",show(cla));
    for(auto &a : cla.atoms) a = eval(a);
    return cla;
  }

  str DebugString() const {
    vec<str> l;
    for(size_t i=0; i<val.size(); ++i) if(val[i]) {
      l.push_back(util::fmt("v% = %",i,show(val[i].get())));
    }
    return util::fmt("{ % }",util::join("; ",l));
  }
};



#endif // MGU_H_
