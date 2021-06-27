#ifndef CONNECTION_TABLEAU_FRAMES_START_H_
#define CONNECTION_TABLEAU_FRAMES_START_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _StartFrame {
  template<typename Div> INL void run(Div *d) const {
    STATE_FRAME(d->A,d->state,"start");
    // TODO: next_starting_clause() has a side effect on state
    // make it more explicit.
    auto dcla = d->state->cla_index.next_starting_clause();
    if(!dcla) return;
    d->alt([&](typename Div::Alt *x)INLL{
      x->feature_branch(Branch());
      x->feature_mcts_node(false);
      x->task(memory::nothing(),[dcla](Div *d)INLL{ strong(d,Branch(),dcla.get(),-1); });
    });
    d->alt([&](typename Div::Alt *x)INLL{
      x->feature_branch(Branch());
      x->feature_mcts_node(false);
      x->task(memory::nothing(),[](Div *d)INLL{ _StartFrame{}.run(d); });
    });
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_START_H_

