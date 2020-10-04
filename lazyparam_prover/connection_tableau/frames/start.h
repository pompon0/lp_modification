#ifndef CONNECTION_TABLEAU_FRAMES_START_H_
#define CONNECTION_TABLEAU_FRAMES_START_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _StartFrame {
  template<typename DState> INL void run(DState *d) const {
    STATE_FRAME(d->A,d->state,"start");
    //TODO: next_starting_clause() has a side effect on state
    // make it more explicit.
    auto dcla = d->state->cla_index.next_starting_clause();
    if(!dcla) return;
    d->or_(Features{.depth=0},[dcla](DState *d)INLL{ strong(d,Branch(),dcla.get(),-1); });
    d->or_(Features{.depth=0},[](DState *d)INLL{ _StartFrame{}.run(d); });
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_START_H_

