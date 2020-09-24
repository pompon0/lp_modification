#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"

namespace tableau::connection_tableau {

// Search takes alloc as an argument to be able to return result in its memory.
alt::SearchResult search(const Ctx &ctx, memory::Alloc &A, SearchState &state, size_t depth_limit) { FRAME("connection_tableau::search()");
  SCOPE("connection_tableau::search");
  struct Save {
    TaskSet cont; // always execute the last
    SearchState::Save ss;
    memory::Alloc::Save As;
    memory::List<memory::Maybe<TaskSet>> actions; // actions left
  };
  auto task = start_task(A);
  auto ss = state.save();
  TaskSet cont(A,task);
  ActionCollector ac(&state);
  run(A,task,&ac);
  vec<Save> saves{Save{cont,ss,A.save(),ac.actions}};
  
  size_t steps = 0;
  for(; saves.size(); steps++) {
    // select action
    if(saves.back().actions.empty()){ saves.pop_back(); continue; }
    auto s = saves.back();
    saves.back().actions = saves.back().actions.tail();
    if(!s.actions.head()) continue;
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    
    // execute action
    A.restore(s.As);
    state.restore(s.ss);
    ActionExecutor ae(&state,s.actions.size()-1);
    run(A,s.cont.head(),&ae);
    auto ss = state.save();
    
    // push back new tasks
    TaskSet ts = s.cont.tail();
    bool ok = true;
    for(auto nt = ae.get(); !nt.empty(); nt = nt.tail()) {
      // skip action if proof tree depth has been exceeded
      if(features(nt.head()).depth>depth_limit) ok = false;
      ts.push(A,nt.head());
    }
    if(ts.empty()) { return {1,steps}; }
    if(!ok) continue;

    // determine next actions
    ActionCollector ac(&state);
    run(A,ts.head(),&ac);
    saves.push_back(Save{ts,ss,A.save(),ac.actions});
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

static ProverOutput prove(const Ctx &ctx, memory::Alloc &A, const ClauseIndex &cla_index, const FunOrd &fun_ord, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(cla_index,fun_ord);
  auto res = search(ctx,A,s,limit);
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
