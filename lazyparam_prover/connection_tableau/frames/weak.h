#ifndef CONNECTION_TABLEAU_FRAMES_WEAK_H_
#define CONNECTION_TABLEAU_FRAMES_WEAK_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

namespace tableau::connection_tableau {

struct WeakFrame {
  Branch branch;

  template<typename DState> INL void weak(memory::Alloc &A, DState *d) const { FRAME("weak(%)",show(branch.false_.head())); 
    d->state->stats.weak_steps++;

    // try to match with lemma
    for(auto b = branch.true_; !b.empty(); b = b.tail()) {
      if(atom_hash!=Index::atom_hash(b.head())) continue;
      if(!d->state->val.equal_mod_sign(a,b.head())) continue;
      if(d->diverge(A,[&]{ return just({}); })) return;
      return;
    }
    if(!a.strong_only()) {
      // try to unify with path
      for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        if(d->diverge(A,[&]{
          if(state->val.unify(A,a,b.head())) return just({});
          return nothing();
        })) return;
      }
      // add constraints (wrt path)
      for(auto b = branch.false_; !b.empty(); b = b.tail())
        if(!state->val.push_constraint(A,OrderAtom::neq(A,a,b.head()))) return alts;
      }
    }

    auto matches = state->cla_index.get_matches(f->branch.false_.head(),nothing());
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      if(d->diverge(A,[&]{ return strong(A,d->state,f->branch,ca.cla,ca.i); })) return;
    }
    return;
  }
};

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_FRAMES_WEAK_H_
