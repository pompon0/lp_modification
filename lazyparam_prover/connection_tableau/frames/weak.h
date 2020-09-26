#ifndef CONNECTION_TABLEAU_FRAMES_WEAK_H_
#define CONNECTION_TABLEAU_FRAMES_WEAK_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _WeakFrame {
  Branch branch;

  template<typename DState> INL void run(memory::Alloc &A, DState *d) const {
    STATE_FRAME(A,d->state,"weak(%)",show(branch.false_.head()));
    d->state->stats.weak_steps++;
    // try to match with lemma
    auto a = branch.false_.head();
    auto atom_hash = Index::atom_hash(a);
    for(auto b = branch.true_; !b.empty(); b = b.tail()) {
      if(atom_hash!=Index::atom_hash(b.head())) continue;
      if(!d->state->val.equal_mod_sign(a,b.head())) continue;
      if(d->diverge(A,[&]{ return memory::just(TaskSet()); })) return;
      return;
    }
    if(!a.strong_only()) {
      // try to unify with path
      for(auto b = branch.false_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        if(d->diverge(A,[&]{
          if(d->state->val.unify(A,a,b.head())) return memory::just(TaskSet());
          return memory::Maybe<TaskSet>();
        })) return;
      }
      // add constraints (wrt path)
      for(auto b = branch.false_.tail(); !b.empty(); b = b.tail()) {
        if(!d->state->val.push_constraint(A,OrderAtom::neq(A,a,b.head()))) return;
      }
    }
    
    // extend
    auto matches = d->state->cla_index.get_matches(branch.false_.head(),memory::nothing());
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      if(d->diverge(A,[&]{ return strong(A,d->state,branch,ca.cla,ca.i); })) return;
    }
    return;
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_WEAK_H_
