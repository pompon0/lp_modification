#ifndef CONNECTION_TABLEAU_FRAMES_START_H_
#define CONNECTION_TABLEAU_FRAMES_START_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

struct _StartFrame {
  template<typename DState> INL void run(memory::Alloc &A, DState *d, size_t size_limit) const {
    STATE_FRAME(A,d->state,"start");
    while(auto dcla = d->state->cla_index.next_starting_clause()) {
      if(d->diverge(A,[&]{ return strong(A,d->state,Branch(),dcla.get(),-1,size_limit); })) return;
    }
    return;
  }
};

#endif  // CONNECTION_TABLEAU_FRAMES_START_H_

