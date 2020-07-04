#ifndef CONSTRAINED_VALUATION_H_
#define CONSTRAINED_VALUATION_H_

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

struct ValuationStats {
  size_t unifications = 0;
  size_t failed_unifications = 0;
  size_t broken_constraints = 0;
  size_t comparisons = 0;
  ValuationStats & operator+=(ValuationStats b) {
    unifications += b.unifications;
    failed_unifications += b.failed_unifications;
    broken_constraints += b.broken_constraints;
    comparisons += b.comparisons;
    return *this;
  }
};

template<typename Ordering> struct ConstrainedValuation {
private:
  List<OrderAtom> constraints;
  Valuation val;
  Ordering ord;
public:
  ConstrainedValuation() : ord(FunOrd()) {}
  ConstrainedValuation(const FunOrd &fun_ord) : ord(fun_ord) {}
  ValuationStats stats;

  Valuation get_valuation() const { return val; }
  size_t size() const { return val.size(); }

  template<typename T> T allocate(T t) {
    t = val.allocate(t);
    ord.resize(val.size());
    return t;
  }

  struct Snapshot {
    Valuation::Snapshot val;
    typename Ordering::Snapshot ord;
    List<OrderAtom> constraints;
  };
  Snapshot snapshot(){
    return {
      val.snapshot(),
      ord.snapshot(),
      constraints
    };
  }
  void rewind(Snapshot s){
    val.rewind(s.val);
    ord.rewind(s.ord);
    constraints = s.constraints;
  }
  
  inline bool equal(Term x, Term y){ return val.equal(x,y); }
  inline bool equal_mod_sign(Atom x, Atom y) { return val.equal_mod_sign(x,y); } 
  template<typename T> T eval(T t) const { return val.eval(t); }
  Term shallow_eval(Term t) const { return val.shallow_eval(t); }
  
  // unifies values, validates constraints afterwards.
  template<typename T> [[nodiscard]] bool unify(T x, T y) { FRAME("mgu()");
    stats.unifications++;
    if(!val.mgu(x,y)){
      stats.failed_unifications++;
      return 0;
    }
    auto s = snapshot();
    if(!check_constraints()){
      stats.broken_constraints++;
      rewind(s);
      return 0;
    }
    return 1;
  }
  
  inline OrderAtom::Relation cmp(Term l, Term r) {
    stats.comparisons++;
    return ord.cmp(val,l,r);
  }

  // returning false invalidates the object 
  [[nodiscard]] bool push_constraint(OrderAtom c) {
    if(c.status()==OrderAtom::TRUE) return 1;
    return check_and_push_constraint(constraints,c);
  }

  [[nodiscard]] bool check_constraints() {
    List<OrderAtom> c2;
    for(auto c = constraints; !c.empty(); c = c.tail()) {
      if(!check_and_push_constraint(c2,c.head())) return false;
    }
    constraints = c2;
    return true;
  } 

  bool check_and_push_constraint(List<OrderAtom> &constraints, OrderAtom c) {
    c = c.reduce(*this);
    switch(c.status()) {
    case OrderAtom::TRUE: return true;
    case OrderAtom::UNKNOWN: constraints += c; return true;
    case OrderAtom::FALSE: return false;
    default: error("c.status() = %",c.status());
    }
  }
};

}  // tableau

#endif // CONSTRAINED_VALUATION_H_
