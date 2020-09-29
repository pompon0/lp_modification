#ifndef CONNECTION_TABLEAU_FRAMES_START_H_
#define CONNECTION_TABLEAU_FRAMES_START_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _StartFrame {
  template<typename DState> INL void run(DState *d) const {
    STATE_FRAME(d->A,d->state,"start");
    //TODO make it divergable
    while(auto dcla = d->state->cla_index.next_starting_clause()) {
      d->or_(Features{.depth=0},[dcla](DState *d)INLL{ return strong(d,Branch(),dcla.get(),-1); });
    }
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_START_H_

