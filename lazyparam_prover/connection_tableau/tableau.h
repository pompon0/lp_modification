#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"
#include "lazyparam_prover/connection_tableau/balanced.h"

namespace tableau::connection_tableau {

struct Div {
  using Task = memory::function<void(Div*)>;
  using Cont = memory::List<Task>;
  enum { size_limit = 100000 };
  
  template<typename F> INL Div(memory::Alloc &_A, SearchState *_state, bool _cut, size_t _depth_limit, F f)
    : A(_A), state(_state), cut(_cut), depth_limit(_depth_limit) { save(Cont(A,Task(A,f))); }

  INL bool step() {
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
INL alt::SearchResult search(const Ctx &ctx, memory::Alloc &A, SearchState &state, bool cut, size_t depth_limit) { FRAME("connection_tableau::search()");
  SCOPE("connection_tableau::search");
  Div d(A,&state,cut,depth_limit,[](Div *d)INLL{ start_task(d); });
  size_t steps = 0;
  for(; d.saves.size(); steps++) {
    if(d.step()) return {1,steps};
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

template<typename SEARCH> INL ProverOutput prove(
  const Ctx &ctx,
  memory::Alloc &A,
  size_t limit_max,
  SEARCH search
) { FRAME("prove_loop()");
  static_assert(memory::has_sig<SEARCH,ProverOutput(const Ctx&, memory::Alloc &, size_t limit)>());
  SCOPE("prove_loop"); 
  Stats stats;
  size_t cont_count = 0;
  size_t limit = 0;
  //info("ClauseIndex begin");
  //info("ClauseIndex end");
  for(;!ctx.done();) {
    if(++limit>limit_max) break; // avoid incrementing limit before context check
    DEBUG info("limit = %",limit);
    ProverOutput out = search(ctx,A,limit);
    out.cont_count += cont_count;
    out.stats += stats;
    if(out.proof) {
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*out.proof));
      return out;
    }
    stats = out.stats;
    cont_count = out.cont_count;
    //std::cerr << "expands[" << limit << "]: " << profile.scopes["expand"].count << std::endl;
  }
  DEBUG info("FAILURE");
  ProverOutput out;
  out.cont_count = cont_count;
  out.cost = limit;
  out.stats = stats;
  return out; 
}

static ProverOutput prove_loop(
  const Ctx &ctx,
  memory::Alloc &A,
  OrForm form,
  const FunOrd &fun_ord
) {
  ClauseIndex idx(form);
  auto out = prove(ctx,A,12,[&](const Ctx &ctx, memory::Alloc &A, size_t limit)INLL{
    // prove
    SearchState s(idx,fun_ord);
    auto As = A.save();
    auto res = balanced::search(ctx,A,s,true,limit);
    s.stats.val = s.val.stats;
    if(!res.found) A.restore(As);
    return ProverOutput{
      res.cont_count,
      limit,
      s.val.get_valuation(),
      res.found ? s.get_proof(A) : 0,
      s.stats,
    };
  });
  if(out.proof) return out;
  return prove(ctx,A,1000000,[&](const Ctx &ctx, memory::Alloc &A, size_t limit){
    // prove
    SearchState s(idx,fun_ord);
    auto As = A.save();
    auto res = balanced::search(ctx,A,s,false,limit);
    s.stats.val = s.val.stats;
    if(!res.found) A.restore(As);
    return ProverOutput{
      res.cont_count,
      limit,
      s.val.get_valuation(),
      res.found ? s.get_proof(A) : 0,
      s.stats,
    };
  });
}

} // namespace connection_tableau::tableau

#endif  // CONNECTION_TABLEAU_TABLEAU_H_
