#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"
#include "lazyparam_prover/connection_tableau/balanced.h"

namespace tableau::connection_tableau {

struct Div {
  using Task = memory::function<void(Div*)>;
  using Cont = memory::List<Task>;
  enum { size_limit = 100000 };
  
  template<typename F> INL Div(memory::Alloc &_A, SearchState *_state, size_t _depth_limit, F f)
    : A(_A), state(_state), depth_limit(_depth_limit) { save(Cont(A,Task(A,f))); }

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
  size_t depth_limit;

  Cont _and;
  template<typename F> INL void or_(Features x, F f){ FRAME("or_"); if(x.depth<=depth_limit) save(_and.add(A,Task(A,f))); }
  template<typename F> INL void and_(F f){ FRAME("and_"); _and.push(A,Task(A,f)); }
  INL void done(Features f){ if(f.depth<=depth_limit) save(_and); }

  INL void save(Cont cont) {
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
INL alt::SearchResult search(const Ctx &ctx, memory::Alloc &A, SearchState &state, size_t depth_limit) { FRAME("connection_tableau::search()");
  SCOPE("connection_tableau::search");
  Div d(A,&state,depth_limit,[](Div *d){ start_task(d); });
  size_t steps = 0;
  for(; d.saves.size(); steps++) {
    if(d.step()) return {1,steps};
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

static ProverOutput prove(const Ctx &ctx, memory::Alloc &A, const ClauseIndex &cla_index, const FunOrd &fun_ord, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(cla_index,fun_ord);
  auto res = balanced::search(ctx,A,s,limit);
  s.stats.val = s.val.stats;
  DEBUG_ONLY(
    if(res.found) {
      str trace;
      size_t i = 0;
      for(auto l=s.trace; !l.empty(); l=l.tail()) {
        trace = util::fmt("[%] %\n",i++,l.head()) + trace;
      }
      info("TRACE = \n%",trace);
    }
  )
  return {
    res.cont_count,
    limit,
    s.val.get_valuation(),
    res.found ? s.get_proof(A) : 0,
    s.stats,
  };
}

static ProverOutput prove_loop(const Ctx &ctx, memory::Alloc &A, OrForm form, const FunOrd &fun_ord) { FRAME("prove_loop()");
  SCOPE("prove_loop"); 
  Stats stats;
  size_t cont_count = 0;
  size_t limit = 0;
  //info("ClauseIndex begin");
  ClauseIndex idx(form);
  //info("ClauseIndex end");
  for(;!ctx.done();) {
    limit++; // avoid incrementing limit before context check
    DEBUG info("limit = %",limit);
    ProverOutput out = prove(ctx,A,idx,fun_ord,limit);
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

} // namespace connection_tableau::tableau

#endif  // CONNECTION_TABLEAU_TABLEAU_H_
