#ifndef CONNECTION_TABLEAU_FRAMES_WEAK_H_
#define CONNECTION_TABLEAU_FRAMES_WEAK_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _WeakFrame {
  Branch branch;

  template<typename Div> INL void run(Div *d) const {
    STATE_FRAME(d->A,d->state,"weak(%)",show(branch.false_.head()));
    PROF_CYCLES("connection_tableau::weak");
    d->state->stats.weak_steps++;
    // try to match with lemma
    auto a = branch.false_.head();
    auto atom_hash = Index::atom_hash(a);
    for(auto b = branch.true_; !b.empty(); b = b.tail()) {
      if(atom_hash!=Index::atom_hash(b.head())) continue;
      if(!d->state->val.equal_mod_sign(a,b.head())) continue;
      d->alt([&](typename Div::Alt *x)INLL{
        x->feature_branch(branch);
        x->feature_mcts_node(false);
      });
      return;
    }
    if(!a.strong_only()) {
      // try to unify with path
      for(auto b = branch.false_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        d->alt([&](typename Div::Alt *x)INLL{
          auto br = branch;
          x->feature_branch(br);
          x->feature_mcts_node(false);
          x->task([br,a,b](Div *d)INLL{
            if(!d->state->val.unify(d->A,a,b.head())) return;
            d->alt([&](typename Div::Alt *x)INLL{
              x->feature_branch(br);
              x->feature_mcts_node(true);
            });
          });
        });
      }
    }
    
    d->alt([&](typename Div::Alt *x)INLL{
      auto br = branch;
      x->feature_branch(br);
      x->feature_mcts_node(false);
      x->task([a,br](Div *d)INLL{
        if(!a.strong_only()) {
          // add constraints (wrt path)
          // Note that we are ignoring the sign here, since
          // - opposite literals should have been unified using the strong connection.
          // - equal literals are not allowed by the regularity constraint.
          for(auto b = br.false_.tail(); !b.empty(); b = b.tail()) {
            if(!d->state->val.push_constraint(d->A,OrderAtom::neq(d->A,a,b.head()))) return;
          }
        }
        
        // extend
        auto matches = d->state->cla_index.get_matches(br.false_.head(),memory::just(d->size_limit() - d->state->nodes_used));
        while(auto mca = matches.next()) {
          auto ca = mca.get();
          d->alt([&](typename Div::Alt *x)INLL{
            x->feature_branch(br);
            x->feature_mcts_node(false);
            x->task([br,ca](Div *d)INLL{ strong(d,br,ca.cla,ca.i); });
          });
        }
      });
    });
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_WEAK_H_
