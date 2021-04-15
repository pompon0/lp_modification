#ifndef ENGINE_DEPTH_H_
#define ENGINE_DEPTH_H_

#include "lazyparam_prover/engine/engine.h"

namespace tableau::engine::depth {

struct Div {
  using Features = tableau::Features;

  using Task = memory::function<void(Div*)>;
  using Cont = memory::List<Task>;
  enum { size_limit = 100000 };
  
  template<typename F> INL Div(memory::Alloc &_A, SearchState *_state, bool _cut, size_t _depth_limit, F f)
    : A(_A), state(_state), cut(_cut), depth_limit(_depth_limit) { save(Cont(A,Task(A,f))); }

  INL bool step() {
    state->stats.steps++;
    auto s = saves.back(); saves.pop_back();
    A.restore(s.As);
    state->restore(s.ss);
    if(s.cont.empty()){ return true; }
    _and = s.cont.tail();
    s.cont.head()(this);
    return false;
  }

  memory::Alloc &A;
  SearchState *state;
  bool cut;
  size_t depth_limit;

  Cont _and;
  template<typename F> INL void or_(Features x, F f){ FRAME("or_"); if(x.depth<=depth_limit) save(_and.add(A,Task(A,f))); }
  template<typename F> INL void and_(F f){ FRAME("and_"); _and.push(A,Task(A,f)); }
  INL void done(Features f){ if(f.depth<=depth_limit) save(_and); }

  INL void save(Cont cont) {
    if(cut) {
      //TODO: add comp(7) phase: restart search without cut
      //TODO: can be optimized
      size_t s = cont.size();
      while(saves.size()) {
        auto c = saves.back().cont;
        auto sc = c.size();
        if(sc<s) break;
        while(sc-->s) c = c.tail();
        if(!cont.pointer_equal(c)) break;
        saves.pop_back();
      }
    }
    saves.push_back({
      .cont = cont,
      .ss = state->save(),
      .As = A.save(),
    });
  }
  struct Save {
    Cont cont;
    SearchState::Save ss;
    memory::Alloc::Save As;
  };
  vec<Save> saves;
};

// Search takes alloc as an argument to be able to return result in its memory.
template<typename Cont> INL bool search(const Ctx &ctx, memory::Alloc &A, SearchState &state, bool cut, size_t depth_limit) { FRAME("connection_tableau::search()");
  SCOPE("connection_tableau::search");
  Div d(A,&state,cut,depth_limit,Cont::start());
  size_t steps = 0;
  for(; d.saves.size(); steps++) {
    if(d.step()) return true;
    if(steps%100==0 && ctx.done()) return false;
    DEBUG if(steps%1000==0) info("steps = %",steps);
  }
  DEBUG info("steps = %",steps);
  return false;
}

template<typename Cont> ProverOutput schedule(
  Ctx::Ptr ctx,
  memory::Alloc &A,
  SearchState &s
) {
  auto d = ctx->get_deadline();
  if(!d) error("deadline required");
  Ctx::Ptr ctx1 = Ctx::with_timeout(ctx,(*d-absl::Now())*0.6);
  Ctx::Ptr ctx2 = ctx;
  auto out = iterative_deepening(*ctx1,A,s,[&](const Ctx &ctx, memory::Alloc &A, SearchState &s, size_t limit)INLL{
    return search<Cont>(ctx,A,s,true,limit);
  });
  if(out.proof) return out;
  return iterative_deepening(*ctx2,A,s,[&](const Ctx &ctx, memory::Alloc &A, SearchState &s, size_t limit)INLL{
    return search<Cont>(ctx,A,s,false,limit);
  });
}

} // namespace tableau::engine

#endif  // ENGINE_DEPTH_H_
