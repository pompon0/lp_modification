#ifndef KBO_H_
#define KBO_H_

#include "lazyparam_prover/util/short.h"
#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/fun_ord.h"
#include "lazyparam_prover/log.h"
#include <algorithm>

namespace tableau {

struct KBO {
private:
  ResetArray<int> var_occ;
  using Res = OrderAtom::Relation;
  FunOrd fun_ord;
public:
  KBO(const FunOrd &_fun_ord) : fun_ord(_fun_ord) {}

  struct Snapshot { size_t var_occ_size; };
  Snapshot snapshot(){ return {var_occ.size()}; }
  void rewind(Snapshot s){ var_occ.resize(s.var_occ_size,0); }
  inline void resize(size_t n) { var_occ.resize(n,0); }

  inline Res cmp(const Valuation &val, Term l, Term r) { FRAME("KBO.cmp()");
    auto res = Ctx(val,*this).cmp(l,r);
    var_occ.reset(0);
    return res;
  }

private:
  struct Ctx {
    explicit Ctx(const Valuation &_val, KBO &_kbo) : val(_val), kbo(_kbo) {}
    const Valuation &val;
    KBO &kbo;
    int pos = 0;
    int neg = 0;
    int weight = 0;
  
    inline Res cmp(u64 l, u64 r) const { return kbo.fun_ord.less(l,r) ? OrderAtom::L : kbo.fun_ord.less(r,l) ? OrderAtom::G : OrderAtom::E; }

    inline void accum(Term t, int f) { FRAME("Balance.accum()");
      t = val.shallow_eval(t);
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
      if(pos && neg) return OrderAtom::U;
      if(pos && !neg) return weight>0 || (weight==0 && lex==OrderAtom::G) ? OrderAtom::G : OrderAtom::U;
      if(!pos && neg) return weight<0 || (weight==0 && lex==OrderAtom::L) ? OrderAtom::L : OrderAtom::U;
      return weight<0 ? OrderAtom::L : weight>0 ? OrderAtom::G : lex;
    }
   
    Res operator()(Term l, Term r){ return cmp(l,r); }

    inline Res cmp(Term l, Term r) { FRAME("Ctx.cmp(%,%)",show(l),show(r));
      //TODO: replace with hash cons
      l = val.shallow_eval(l);
      r = val.shallow_eval(r);
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
            if(lex==OrderAtom::E) continue;
            while(++i<ac){ accum(lf.arg(i),1); accum(rf.arg(i),-1); }
            return cmp_accum(lex);
          }
          return OrderAtom::E;
        }
      } else {
        //TODO: replace with hash cons
        accum(l,1);
        accum(r,-1);
        return l==r ? OrderAtom::E : cmp_accum(OrderAtom::U);
      }
    }
  };
};

}  // tableau

#endif // KBO_H_
