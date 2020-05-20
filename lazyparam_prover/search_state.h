#ifndef SEARCH_STATE_H_
#define SEARCH_STATE_H_

#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/lpo.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

namespace tableau {

struct Branch {
  List<Atom> false_;
  List<Atom> true_;
};

inline str show(Branch b) {
  vec<str> atoms;
  for(auto bt = b.false_; !bt.empty(); bt = bt.tail()) atoms.push_back(show(bt.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

using Val = ConstrainedValuation<LPO>;

struct SearchState {
  SearchState(const ClauseIndex &_cla_index) : cla_index(&_cla_index) {}
 
  ClauseIndex::State cla_index;

  Val val;
  size_t nodes_used = 0;
  List<DerAndClause> clauses_used;

  Stats stats;

  AndClause allocate(DerAndClause dcla) { FRAME("strong_unify()");
    dcla = val.allocate(dcla);
    clauses_used += dcla;
    nodes_used += dcla.cost();
    for(size_t i=dcla.constraint_count(); i--;){
      val.push_constraint(dcla.constraint(i));
    }
    return dcla.derived();
  }

  // cannot return the proto, because parsing context is not available.
  // This means that Valuation has to be included in the ProverOutput.
  ptr<OrForm> get_proof() {
    auto proof = util::make<OrForm>();
    for(auto l=clauses_used; !l.empty(); l = l.tail()) {
      proof->and_clauses.push_back(l.head());
    }
    return proof;
  }

  struct Snapshot {
    Val::Snapshot val;
    tableau::Snapshot stack;
    size_t nodes_used;
    List<DerAndClause> clauses_used;
    ClauseIndex::State cla_index;
  };

  void rewind(Snapshot s) {
    val.rewind(s.val);
    stack = s.stack;
    nodes_used = s.nodes_used;
    clauses_used = s.clauses_used;
    cla_index = s.cla_index;
  }

  Snapshot snapshot(){
    return {val.snapshot(),stack,nodes_used,clauses_used,cla_index};
  }
};

}  // namespace tableau

#endif  // SEARCH_STATE_H_
