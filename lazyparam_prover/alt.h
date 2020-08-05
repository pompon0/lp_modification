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

template<typename E> List<E> concat(memory::Alloc &A, List<E> a, List<E> b) { FRAME("concat");
  return a.empty() ? b : concat(A,a.tail(),b).add(A,a.head());
}

template<typename Cont> SearchResult search(const Ctx &ctx, memory::Alloc &_A, Cont c) { FRAME("search()");
  memory::Alloc SA;
  SCOPE("alt::search");
  List<Cont> conts(SA,c);
  size_t steps = 0;
  for(; !conts.empty(); steps++) {
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    auto c = conts.head(); conts = conts.tail();
    if(c.done()) return {1,steps};
    auto x = c.run();
    info("x.head().frames.empty() == %",x.head().frames.empty());
    conts = concat(SA,x,conts);
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

} // namespace alt
} // namespace tableau

#endif  // LAZYPARAM_PROVER_ALT_H_
