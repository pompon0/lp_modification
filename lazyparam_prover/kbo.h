#ifndef KBO_H_
#define KBO_H_

#include "lazyparam_prover/util/short.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/alloc.h"
#include "lazyparam_prover/mgu.h"
#include <algorithm>

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
  // returning false invalidates the object
  inline bool mgu(Term x, Term y) {
    if(!val.mgu(x,y)) return false;
    if(!check_constraints()) return false; 
    return true;
  } 
  inline OrClause eval(OrClause cla) { return val.eval(cla); }
  inline bool opposite(Atom x, Atom y) { return val.opposite(x,y); }
  
  enum Res { L, G, E, N };
  inline Res cmp(Term l, Term r) { FRAME("KBO.cmp()");
    auto res = Ctx(*this).cmp(l,r);
    var_occ.reset(0);
    return res;
  }
  List<Constraint> constraints;
  void push_not_opposite_constraint(Atom l, Atom r) {
    if(l.pred()!=r.pred() || l.sign()==r.sign()) return;
    DEBUG if(l.arg_count()!=r.arg_count()) error("l.arg_count() = %, r.arg_count() = %",show(l),show(r));
    List<Constraint::Pair> p;
    for(size_t i=l.arg_count(); i--;) p += {l.arg(i),r.arg(i)};
    constraints += {Constraint::NEQ,p};
  }
private:
  ResetArray<int> var_occ;
  Valuation val; 

  bool check_constraints() {
    List<Constraint> c2;
    for(auto c = constraints; !c.empty(); c = c.tail()) {
      auto ch = c.head();
      switch(ch.type) {
        case Constraint::NEQ: {
          auto p = ch.or_;
          bool done = false;
          for(; !done && !p.empty(); p = p.tail()) {
            auto ph = p.head();
            switch(cmp(ph.l,ph.r)) {
              case KBO::E: break;
              case KBO::N:
                c2 += {ch.type,p};
                done = true;
                break;
              default:
                done = true;
                break;
            }
            if(!done) return false;
          }
          break;
        }
        case Constraint::LT: {
          DEBUG if(ch.or_.size()!=1) error("ch.or_.size() = %, want %",ch.or_.size(),1);
          auto ph = ch.or_.head(); 
          switch(cmp(ph.l,ph.r)) {
            case KBO::L: break;
            case KBO::N: c2 += ch;
            default: return false;
          }
        }
      }
    }
    constraints = c2;
    return true;
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

#endif // KBO_H_
