#ifndef LPO_H_
#define LPO_H_

#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/fun_ord.h"
#include "utils/log.h"

namespace tableau {

// a <LPO b <=>
//  (\exists i : a <LPO b_i  or  a = b_i) or
//  (a = (a_f,a_1,..) <lex(LPO) (b_f,b_1,...) = b  and  a_i <LPO b)
// Defined inductively on |a|+|b|
//
// a <LPO b <LPO c  =>  a <LPO c
//  (?) (1)
//    a <LPO b <LPO c_i  (induction)=>  a <LPO c_i  =>  a<LPO c
//    a <LPO b = c_i  =>  a <LPO c
//  (1) (2)
//    a <LPO b_i <LPO c  (induction)=>  a <LPO c
//    a = b_i <LPO c  =>  a <LPO c
//  (2) (2)
//    a <lex(LPO) b <lex(LPO) c  =>  a <lex(LPO) c
//      and  a_i <LPO b <LPO c  (induction)=>  a_i <LPO c
//
// a <subterm b => a <LPO b
//   by (1)
//
// a </LPO a
//  by induction a </lex(LPO) a
//  a_i <subterm a <LPO a_i  (transitivity)=>  a_i <LPO a_i
//    contradiction with inductive assumption
// a <LPO b  =>  b </LPO a
//  otherwise a <LPO a
//
// ground a,b  =>  a <LPO b  or  b <LPO a
//  Assume (1) doesn't hold in any direction. Then by induction:
//  b_i <LPO a  and  a_i <LPO b
//  And by induction either (a <lex(LPO) b) or (b <lex(LPO) a)
//
// Equivalent definition
//  a <LPO b <=> (
//    a_i = b_i  for i<k
//    a_i <LPO b_i  for i=k
//    a_i <LPO b  for i>k
//  ) or (\exists i : a <LPO b_i  or  a = b_i)
//
// Note that
//   a_i = b_i  =>  !(a <LPO b_i  or  a = b_i)
//   a_i </LPO b_i  =>  !(a <LPO b_i  or  a = b_i)
//   a_i </LPO b  =>  !(\exists j: a <LPO b_j  or  a = b_j)
// so it is straightforward to implement <LPO with time complexity O(|a||b|).
//
// =======================================
//  For bidirectional comparison, part of the computation can be deduplicated.
//  a_i <LPO b_i  for i=k
//    L: \forall a_i <LPO b  for i>k
//    G: \exists i>k : b <LPO a_i  or  b = a_i
//  b_i <LPO a_i  for i=k
//    L: \exists i>k : a <LPO b_i  or  a = b_i
//    G: \forall b_i <LPO a  for i>k
//  a_i </LPO b_i  and  b_i </LPO a_i  for i=k
//    L: \exists i>k : a <LPO b_i  or  a = b_i (!)
//    G: \exists i>k : b <LPO a_i  or  b = a_i (!)
//
// =======================================
// \forall {ground valuation V}: V(a) <LPO V(b)
// doesn't imply a <LPO b
// For example, for
//  a = f(X,Y)
//  b = f(Y,f(X,X))
// we have
//  a <\LPO b_1, since Y <LPO f(X,Y)
//  a <\LPO b_2, since f(X,Y) and f(X,X) are unifiable
//  a_1 /= b_1 and a_1 </LPO b_1
// therefore a </LPO b
// On the other hand, given any ground valuation V
//  V(X) <LPO V(Y)  =>  V(a_1) <LPO V(b_1) and a_2 <LPO b  =>  V(a) < V(b)
//  V(X) = V(Y)  =>  V(a_1) = V(b_1) and V(a_2) < V(b_2)  => V(a) < V(b)
//  V(X) >LPO V(Y)  =>  V(a) < V(b_2)  =>  V(a) < V(b)
struct LPO {
  struct Save {};
  Save save(){ return {}; };
  void restore(Save){}
  void resize(size_t){}

  OrderAtom::Relation cmp(const Valuation &val, Term l, Term r) { return Ctx(val,fun_ord)(l,r); }
  LPO(FunOrd _fun_ord) : fun_ord(_fun_ord) {}
private:
  FunOrd fun_ord;

  struct Ctx {
    explicit Ctx(const Valuation &_val, const FunOrd &_fun_ord) : val(_val), fun_ord(_fun_ord) {}
    const Valuation &val;
    const FunOrd &fun_ord;

    OrderAtom::Relation operator()(Term a, Term b) {
      return 
        val.equal(a,b) ? OrderAtom::E :
        less(a,b) ? OrderAtom::L :
        less(b,a) ? OrderAtom::G :
        OrderAtom::U;
    }
    
    bool less(Fun a, Fun b){ 
      bool lex = fun_ord.less(a.fun(),b.fun());
      size_t i=0;
      if(a.fun()==b.fun()) {
        size_t n = a.arg_count();
        DEBUG if(a.arg_count()!=b.arg_count()) error("arg_count() mismatch");
        for(;i<n; i++) if(!val.equal(a.arg(i),b.arg(i))) break;
        if(i==n) return false; 
        lex = less(a.arg(i),b.arg(i));
        i++;
      }
      if(lex){
        for(size_t n = a.arg_count(); i<n; i++) if(!less(a.arg(i),Term(b))) return false;
        return true;
      } else {
        for(size_t n = b.arg_count(); i<n; i++) if(val.equal(Term(a),b.arg(i)) || less(Term(a),b.arg(i))) return true;
        return false;
      }
    }

    bool less(Term a, Term b) { FRAME("less(%,%)",show(a),show(b));
      b = val.shallow_eval(b);
      switch(b.type()) {
        case Term::VAR: return false;
        case Term::FUN:
          a = val.shallow_eval(a);
          switch(a.type()) {
            case Term::VAR: return val.has_var(b,Var(a).id());
            case Term::FUN: return less(Fun(a),Fun(b));
            default: error("a.type() = %",b.type());
          }
        default: error("b.type() = %",a.type());
      }
    }
  };
};

} // namespace tableau

#endif  // LPO_H_
