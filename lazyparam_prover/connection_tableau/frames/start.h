#ifndef CONNECTION_TABLEAU_FRAMES_START_H_
#define CONNECTION_TABLEAU_FRAMES_START_H_

#include "lazyparam_prover/connection_tableau/frames/strong.h"

namespace tableau::connection_tableau {

struct StartFrame {
  template<typename DState> INL void start(memory::Alloc &A, DState *d) const { FRAME("start");
    while(auto dcla = state->cla_index.next_starting_clause()) {
      if(d->diverge(A,[&]{ return strong(A,d->state,Branch(),dcla.get(),-1); })) return;
    }
    return;
  }
};

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_FRAMES_START_H_

