#ifndef LAZYPARAM_PROVER_ALT_H_
#define LAZYPARAM_PROVER_ALT_H_

#include "utils/ctx.h"

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

template<typename Cont> SearchResult search(const Ctx &ctx, memory::Alloc &A, Cont c) { FRAME("search()");
  SCOPE("alt::search");
  struct Save {
    List<Cont> conts; 
    memory::Alloc::Save A;
  };
  List<Cont> start(A,c);
  vec<Save> saves{Save{start,A.save()}};
  size_t steps = 0;
  for(; saves.size(); steps++) {
    auto conts = saves.back().conts;
    if(!conts.size()){ saves.pop_back(); continue; }
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    A.restore(saves.back().A);
    auto c = conts.head();
    saves.back().conts = conts.tail();
    if(c.done()) return {1,steps};
    conts = c.run(A);
    saves.push_back(Save{conts,A.save()});
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

} // namespace alt
} // namespace tableau

#endif  // LAZYPARAM_PROVER_ALT_H_
