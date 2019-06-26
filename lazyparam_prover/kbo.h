#ifndef KBO_H_
#define KBO_H_

#include "lazyparam_prover/util/short.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include <algorithm>

inline size_t max_var(Term t) {
  switch(t.type()) {
  case Term::VAR: return Var(t).id();
  case Term::FUN: {
    Fun tf(t);
    size_t h = 0; for(auto i=tf.arg_count(); i--;) util::maxi(h,max_var(tf.arg(i)));
    return h;
  }
  }
  error("max_var(<type = %>)",t.type());
}

struct Balance {
  Balance(size_t var_count) : var_occ(var_count,0), pos(0), neg(0), weight(0) {}
  vec<int> var_occ;
  int pos;
  int neg;
  int weight;

  enum Res { L, G, E, N };
  static inline Res cmp(u64 l, u64 r) { return l<r ? L : l>r ? G : E; }
  
  inline void accum(Term t, int f) { FRAME("Balance.accum()");
    switch(t.type()) {
      case Term::VAR: {
      weight += f;
      auto &x = var_occ[Var(t).id()], x2 = x+f;
      pos += (x2>0)-(x>0);
      neg += (x2<0)-(x<0);
      x = x2;
    }
    case Term::FUN: {
      Fun tf(t);
      weight += f;
      for(auto i=tf.arg_count(); i--;) accum(tf.arg(i),f);
    }
    }
    error("accum(<type=%>,f)",t.type());
  }

  inline Res cmp_accum(Res lex) { FRAME("cmp_accum");
    if(pos && neg) return N;
    if(pos && !neg) return weight>0 || (weight==0 && lex==G) ? G : N;
    if(!pos && neg) return weight<0 || (weight==0 && lex==L) ? L : N;
    return weight<0 ? L : weight>0 ? G : lex;
  }
  
  inline Res cmp(Term l, Term r) { FRAME("Balance.cmp()");
    //TODO: replace with hash cons
    if(l.type()==Term::FUN && r.type()==Term::FUN && l!=r) {
      Fun lf(l), rf(r);
      if(lf.fun()!=rf.fun()) {
        for(auto i=lf.arg_count(); i--;) accum(lf.arg(i),1);
        for(auto i=rf.arg_count(); i--;) accum(rf.arg(i),-1);
        return cmp_accum(cmp(lf.fun(),rf.fun()));
      } else {
        auto ac = lf.arg_count();
        //TODO: replace with hash cons
        size_t i=0; for(;i<ac; ++i) if(lf.arg(i)!=rf.arg(i)) break;
        Res lex = cmp(lf.arg(i),rf.arg(i));
        for(;i<ac; ++i){ accum(lf.arg(i),1); accum(rf.arg(i),-1); }
        return cmp_accum(lex);
      }
    } else {
      //TODO: replace with hash cons
      accum(l,1);
      accum(r,1);
      return l==r ? E : cmp_accum(N);
    }
  }
};

inline bool kbo(Term l, Term r) { FRAME("kbo()");
  return Balance(std::max(max_var(l),max_var(r))+1).cmp(l,r)==Balance::L;
}

#endif // KBO_H_
