#ifndef CONSTRAINED_VALUATION_H_
#define CONSTRAINED_VALUATION_H_

#include "utils/log.h"
#include "utils/short.h"
#include "utils/string.h"
#include "utils/types.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/valuation.h"
#include "lazyparam_prover/fun_ord.h"
#include <algorithm>

namespace tableau {

struct ValuationStats {
  size_t unifications = 0;
  size_t failed_unifications = 0;
  size_t broken_constraints = 0;
  size_t comparisons = 0;
  INL ValuationStats & operator+=(ValuationStats b) {
    unifications += b.unifications;
    failed_unifications += b.failed_unifications;
    broken_constraints += b.broken_constraints;
    comparisons += b.comparisons;
    return *this;
  }
};

template<typename Ordering> struct ConstrainedValuation {
private:
  memory::List<OrderAtom> constraints;
  Valuation val;
  Ordering ord;
public:
  INL ConstrainedValuation() : ord(FunOrd()) {}
  INL ConstrainedValuation(const FunOrd &fun_ord) : ord(fun_ord) {}
  ValuationStats stats;

  INL Valuation get_valuation() const { return val; }
  INL size_t size() const { return val.size(); }

  template<typename T> INL T allocate(T t) {
    t = val.allocate(t);
    ord.resize(val.size());
    return t;
  }

  struct Save {
    Valuation::Save val;
    typename Ordering::Save ord;
    memory::List<OrderAtom> constraints;
  };
  
  INL Save save(){
    PROF_CYCLES("ConstrainedValuation::save");
    return {
      val.save(),
      ord.save(),
      constraints
    };
  }
  INL void restore(Save s){
    PROF_CYCLES("ConstrainedValuation::restore");
    val.restore(s.val);
    ord.restore(s.ord);
    constraints = s.constraints;
  }
  
  INL inline bool equal(Term x, Term y){ return val.equal(x,y); }
  INL inline bool equal_mod_sign(Atom x, Atom y) { return val.equal_mod_sign(x,y); } 
  template<typename T> INL T eval(memory::Alloc &A, T t) const { return val.eval(A,t); }
  INL Term shallow_eval(Term t) const { return val.shallow_eval(t); }
  
  // unifies values, validates constraints afterwards.
  template<typename T> [[nodiscard]] INL bool unify(memory::Alloc &A, T x, T y) { FRAME("unify()");
    PROF_CYCLES("ConstrainedValuation::unify()");
    stats.unifications++;
    if(!val.unify(x,y)){
      stats.failed_unifications++;
      return 0;
    }
    auto s = save();
    if(!check_constraints(A)){
      stats.broken_constraints++;
      restore(s);
      return 0;
    }
    return 1;
  }
  
  INL OrderAtom::Relation cmp(Term l, Term r) {
    stats.comparisons++;
    return ord.cmp(val,l,r);
  }

  // returning false invalidates the object 
  [[nodiscard]] INL bool push_constraint(memory::Alloc &A, OrderAtom c) {
    PROF_CYCLES("ConstrainedValuation::push_constraint()");
    if(c.status()==OrderAtom::TRUE) return 1;
    return check_and_push_constraint(A,constraints,c);
  }

  [[nodiscard]] INL bool check_constraints(memory::Alloc &A) {
    PROF_CYCLES("ConstrainedValuation::check_constraints()");
    memory::List<OrderAtom> c2;
    for(auto c = constraints; !c.empty(); c = c.tail()) {
      if(!check_and_push_constraint(A,c2,c.head())) return false;
    }
    constraints = c2;
    return true;
  } 

  INL bool check_and_push_constraint(memory::Alloc &A, memory::List<OrderAtom> &constraints, OrderAtom c) {
    c = c.reduce(*this);
    switch(c.status()) {
    case OrderAtom::TRUE: return true;
    case OrderAtom::UNKNOWN: constraints.push(A,c); return true;
    case OrderAtom::FALSE: return false;
    default: error("c.status() = %",c.status());
    }
  }
};

}  // tableau

#endif // CONSTRAINED_VALUATION_H_
