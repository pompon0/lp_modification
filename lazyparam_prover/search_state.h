#ifndef SEARCH_STATE_H_
#define SEARCH_STATE_H_

#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/memory/lazy.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/lpo.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

#ifdef DEBUG_MODE
  #define STATE_FRAME(state,args...)\
    auto _msg = util::fmt(args);\
    util::Frame _(VERBOSE,args);\
    state.trace += _msg;
#else
  #define STATE_FRAME(state,args...)
#endif

namespace tableau {

struct Branch {
  List<Atom> false_;
  List<Atom> true_;
};

struct BranchSet {
  Branch branch;
  List<Branch> branches;
  size_t branches_size;

  void push(memory::Alloc &A, Atom a) { FRAME("BranchSet::push(%)",show(a));
    auto b = branch; b.false_.push(A,a); branches.push(A,b);
    branch.true_.push(A,a);
    branches_size++;
  }
};

inline str show(Branch b) {
  vec<str> atoms;
  for(auto bt = b.false_; !bt.empty(); bt = bt.tail()) atoms.push_back(show(bt.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

using Val = ConstrainedValuation<LPO>;

struct SearchState {
  SearchState(
    memory::Alloc &_A,
    const ClauseIndex &_cla_index,
    const FunOrd &fun_ord
  ) : A(_A), cla_index(&_cla_index), val(A,fun_ord) {}
 
  memory::Alloc &A;
  ClauseIndex::State cla_index;

  Val val;
  size_t nodes_used = 0;
  List<DerAndClause> clauses_used;
  List<Lazy<DerAndClause>> lazy_clauses_used;
  DEBUG_ONLY(List<str> trace;)
  
  Stats stats;

  Maybe<AndClause> allocate(DerAndClause dcla) { FRAME("SearchState::allocate()");
    dcla = val.allocate(dcla);
    clauses_used.push(A,dcla);
    nodes_used += dcla.cost();
    for(size_t i=dcla.constraint_count(); i--;){
      if(!val.push_constraint(dcla.constraint(i))) return nothing();
    }
    return just(dcla.derived());
  }

  // cannot return the proto, because parsing context is not available.
  // This means that Valuation has to be included in the ProverOutput.
  ptr<OrForm> get_proof(memory::Alloc &A) {
    auto proof = util::make<OrForm>();
    for(auto l=clauses_used; !l.empty(); l = l.tail()) {
      proof->and_clauses.push_back(l.head());
    }
    for(auto l=lazy_clauses_used; !l.empty(); l = l.tail()) {
      proof->and_clauses.push_back(l.head().get(A));
    }
    return proof;
  }

  struct Save {
    memory::Alloc::Save A;
    Val::Save val;
    size_t nodes_used;
    List<DerAndClause> clauses_used;
    List<Lazy<DerAndClause>> lazy_clauses_used;
    ClauseIndex::State cla_index;
    DEBUG_ONLY(List<str> trace;)
  };

  void restore(Save s) {
    A.restore(s.A);
    val.restore(s.val);
    nodes_used = s.nodes_used;
    clauses_used = s.clauses_used;
    lazy_clauses_used = s.lazy_clauses_used;
    cla_index = s.cla_index;
    DEBUG_ONLY(trace = s.trace;)
  }

  Save save(){
    return {
      .A = A.save(),
      .val = val.save(),
      .nodes_used = nodes_used,
      .clauses_used = clauses_used,
      .lazy_clauses_used = lazy_clauses_used,
      .cla_index = cla_index,
      DEBUG_ONLY(.trace = trace,)
    };
  }
};

}  // namespace tableau

#endif  // SEARCH_STATE_H_
