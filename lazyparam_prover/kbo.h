#ifndef KBO_H_
#define KBO_H_

#include "lazyparam_prover/util/short.h"
#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/alloc.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/log.h"
#include <algorithm>

namespace tableau {

struct Constraint {
  enum Type { NEQ, LT };
  Type type;
  struct Pair { Term l,r; };
  List<Pair> or_;
};

struct KBO {
public:
  size_t size(){ return val.size(); }
  void resize(size_t n){ val.resize(n); var_occ.resize(n,0); }
  struct Snapshot {
    Valuation::Snapshot val;
    List<Constraint> constraints;
  };
  Snapshot snapshot(){ return {val.snapshot(),constraints}; }
  void rewind(Snapshot s){ val.rewind(s.val); constraints = s.constraints; }
  
  inline bool equal(Term x, Term y){ return val.equal(x,y); }
  inline bool equal_mod_sign(Atom x, Atom y) { return val.equal_mod_sign(x,y); } 
  inline OrClause eval(OrClause cla) { return val.eval(cla); }
  
  // unifies atoms ignoring the sign, validates constraints afterwards 
  // returning false invalidates the object 
  inline bool mgu(Atom x, Atom y) { FRAME("mgu()");
    SCOPE("Valuation::mgu(Atom)");
    if(x.pred()!=y.pred()) return 0;
    DEBUG if(x.arg_count()!=y.arg_count()) error("arg_count() mismatch: %, %",show(x),show(y));
    auto s = snapshot();
    for(size_t i=x.arg_count(); i--;)
      if(!val.mgu(x.arg(i),y.arg(i))){ rewind(s); return 0; }

    if(!check_constraints()) return 0;
    return 1;
  }
  
  enum Res { L, G, E, N };
  inline Res cmp(Term l, Term r) { FRAME("KBO.cmp()");
    auto res = Ctx(*this).cmp(l,r);
    var_occ.reset(0);
    return res;
  }

  List<Constraint> constraints;
  // ignores sign
  // returning false invalidates the object 
  bool push_constraint(Atom l, Atom r) { FRAME("push_constraint(%,%)",show(l),show(r));
    if(l.pred()!=r.pred()) return 1;
    DEBUG if(l.arg_count()!=r.arg_count()) error("l.arg_count() = %, r.arg_count() = %",show(l),show(r));
    List<Constraint::Pair> p;
    for(size_t i=l.arg_count(); i--;) p += {l.arg(i),r.arg(i)};
    return check_and_push_constraint_with_log(constraints,{Constraint::NEQ,p});
  }
private:
  ResetArray<int> var_occ;
  Valuation val; 

  bool check_constraints() {
    List<Constraint> c2;
    for(auto c = constraints; !c.empty(); c = c.tail()) {
      if(!check_and_push_constraint_with_log(c2,c.head())) return false;
    }
    constraints = c2;
    return true;
  } 

  bool check_and_push_constraint_with_log(List<Constraint> &constraints, Constraint c) {
    if(check_and_push_constraint(constraints,c)) return 1;
    DEBUG {
      info("val = %",val.DebugString());
      for(auto c = constraints; !c.empty(); c = c.tail()) { 
        vec<str> ps;
        for(auto p = c.head().or_; !p.empty(); p = p.tail()) {
          ps.push_back(util::fmt("[% = %, % = %]",show(p.head().l),show(val.eval(p.head().l)),show(p.head().r),show(val.eval(p.head().r))));
        }
        str ts = c.head().type==Constraint::NEQ ? "!=" : "<";
        info("% :: %",ts,util::join(" ",ps));
      }
    }
    return 0;
  }

  bool check_and_push_constraint(List<Constraint> &constraints, Constraint c) { FRAME("check_and_push_constraint");
    switch(c.type) {
      case Constraint::NEQ: {
        for(auto p = c.or_; !p.empty(); p = p.tail()) {
          auto ph = p.head();
          switch(cmp(ph.l,ph.r)) {
            case KBO::E: break;
            case KBO::N:
              constraints += {c.type,p};
              return true;
            default:
              return true;
          }
        }
        return false;
      }
      case Constraint::LT: {
        DEBUG if(c.or_.size()!=1) error("c.or_.size() = %, want %",c.or_.size(),1);
        auto ph = c.or_.head(); 
        switch(cmp(ph.l,ph.r)) {
          case KBO::L: return true;
          case KBO::N: constraints += c; return true;
          default: return false;
        }
      }
    }
    error("c.type() = %",c.type);
  }

  struct Ctx {
    explicit Ctx(KBO &_kbo) : kbo(_kbo) {}
    KBO &kbo;
    int pos = 0;
    int neg = 0;
    int weight = 0;
  
    static inline Res cmp(u64 l, u64 r) { return l<r ? L : l>r ? G : E; }

    inline void accum(Term t, int f) { FRAME("Balance.accum()");
      t = kbo.val.shallow_eval(t);
      switch(t.type()) {
        case Term::VAR: {
          weight += f;
          auto vi = Var(t).id();
          auto x = kbo.var_occ[vi], x2 = x+f;
          pos += (x2>0)-(x>0);
          neg += (x2<0)-(x<0);
          kbo.var_occ.set(vi,x2);
          break;
        }
        case Term::FUN: {
          Fun tf(t);
          weight += f;
          for(auto i=tf.arg_count(); i--;) accum(tf.arg(i),f);
          break;
        }
        default:
          error("accum(<type=%>,f)",t.type());
      }
    }

    inline Res cmp_accum(Res lex) { FRAME("cmp_accum");
      if(pos && neg) return N;
      if(pos && !neg) return weight>0 || (weight==0 && lex==G) ? G : N;
      if(!pos && neg) return weight<0 || (weight==0 && lex==L) ? L : N;
      return weight<0 ? L : weight>0 ? G : lex;
    }
    
    inline Res cmp(Term l, Term r) { FRAME("Ctx.cmp(%,%)",show(l),show(r));
      //TODO: replace with hash cons
      l = kbo.val.shallow_eval(l);
      r = kbo.val.shallow_eval(r);
      if(l.type()==Term::FUN && r.type()==Term::FUN && l!=r) {
        Fun lf(l), rf(r);
        if(lf.fun()!=rf.fun()) {
          accum(l,1);
          accum(r,-1);
          return cmp_accum(cmp(lf.fun(),rf.fun()));
        } else {
          DEBUG if(lf.arg_count()!=rf.arg_count())
            error("lf.arg_count() = %, rf.arg_count() = %",lf.arg_count(),rf.arg_count());
          auto ac = lf.arg_count();
          //TODO: replace with hash cons
          for(size_t i=0; i<ac; ++i) {
            Res lex = cmp(lf.arg(i),rf.arg(i));
            if(lex==E) continue;
            while(++i<ac){ accum(lf.arg(i),1); accum(rf.arg(i),-1); }
            return cmp_accum(lex);
          }
          return E;
        }
      } else {
        //TODO: replace with hash cons
        accum(l,1);
        accum(r,-1);
        return l==r ? E : cmp_accum(N);
      }
    }
  };
};

}  // tableau

#endif // KBO_H_
