#ifndef LAZYPARAM_PROVER_ALT_H_
#define LAZYPARAM_PROVER_ALT_H_

#include "lazyparam_prover/ctx.h"

namespace tableau {
namespace alt {

struct ExampleCont {
  bool done() const { return 1; }
  List<ExampleCont> run() const { return nothing(); }
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
  size_t steps = 0;
  for(; !alts.empty(); steps++) {
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
  DEBUG info("steps = %",steps);
  return {0,cont_count};
}

} // namespace alt
} // namespace tableau

#endif  // LAZYPARAM_PROVER_ALT_H_
