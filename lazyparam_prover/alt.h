#ifndef LAZYPARAM_PROVER_ALT_H_
#define LAZYPARAM_PROVER_ALT_H_

#include "lazyparam_prover/ctx.h"

namespace alt {

struct ExampleCont {
  struct State {
    struct Snapshot {};
    void rewind(Snapshot){}
    Snapshot snapshot(){ return {}; }
  };
  bool done() const { return 1; }
  template<typename Alts> void run(State &state, Alts &alts) const {}
};

struct SearchResult {
  bool found;
  size_t cont_count;
};

template<typename Cont> SearchResult search(const Ctx &ctx, typename Cont::State &state, Cont c) {
  SCOPE("alt::search");
  struct Alt { Cont cont; typename Cont::State::Snapshot snapshot; };
  size_t cont_count = 0;
  List<Alt> alts({c,state.snapshot()});
  for(size_t steps = 0; !alts.empty(); steps++) {
    if(steps%100==0 && ctx.done()) return {0,cont_count};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    auto a = alts.head(); alts = alts.tail();
    state.rewind(a.snapshot);
    if(a.cont.done()) return {1,cont_count};
    a.cont.run(state,[&alts,&state,&cont_count](Cont cont){
      cont_count++;
      alts = Alt{cont,state.snapshot()} + alts;
    });
  }
  return {0,cont_count};
}

} // namespace alt

#endif  // LAZYPARAM_PROVER_ALT_H_
