#ifndef CONNECTION_TABLEAU_FRAMES_START_H_
#define CONNECTION_TABLEAU_FRAMES_START_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _StartFrame {
  Features features() const {
    return {
      .depth = 0,
    };
  }
  template<typename DState> INL void run(memory::Alloc &A, DState *d) const {
    STATE_FRAME(A,d->state,"start");
    while(auto dcla = d->state->cla_index.next_starting_clause()) {
      if(d->diverge(A,[&]{ return strong(A,d->state,Branch(),dcla.get(),-1); })) return;
    }
    return;
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_START_H_

