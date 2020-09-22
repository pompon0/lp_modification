#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"

namespace tableau::connection_tableau {

struct Features {};

struct ActionCollector {
  ActionCollector(SearchState *_state) : state(_state), diverging(false) {}
  SearchState *state;
  [[nodiscard]] INL bool diverge(memory::Alloc &A, std::function<TaskSet(void)> f) {
    DEBUG if(diverging) error("nested diverge() not supported");
    auto s = state->save();
    if(f()) {
      //TODO: collect features
    }
    // push a tombstone anyway, so that we have a correct count in actionexecutor
    actions.push(A,{});
    state->restore(s);
    return false;
  }
  size_t count() { return actions.size(); }
private:
  bool diverging;
  List<Features> actions;
};

struct ActionExecutor {
  ActionExecutor(SearchState *_state, int _skip_count) : state(_state), skip_count(_skip_count) {}
  SearchState *state;
  [[nodiscard]] INL bool diverge(memory::Alloc &A, std::function<TaskSet(void)> f) {
    DEBUG if(diverging) error("nested diverge() not supported");
    if(skip_count--) return false;
    task_set = f();
    return true;
  }
  TaskSet get() {
    DEBUG if(skip_count!=-1) error("action not found");
    return task_set;
  }
private:
  int skip_count;
  TaskSet task_set;
};

static Task start_task(memory::Alloc &A) {
  return Task(Task::Start::Builder(A).build());
}

// Search takes alloc as an argument to be able to return result in its memory.
alt::SearchResult search(const Ctx &ctx, memory::Alloc &A, SearchState &state) { FRAME("connection_tableau::search()");
  SCOPE("connection_tableau::search");
  struct Save {
    TaskSet cont; // always execute the last
    memory::State::Save ss;
    memory::Alloc::Save As;
    size_t action_count; // actions left
  };
  auto task = start_task(A);
  TaskSet cont(A,t);
  ActionCollector ac(state);
  task.run(A,&ac);
  vec<Save> saves{Save{cont,state.save(),A.save(),ac.count()}};
  
  size_t steps = 0;
  for(; saves.size(); steps++) {
    // select action
    auto s = saves.back();
    if(!saves.back().action_count--){ saves.pop_back(); continue; }
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    
    // execute action
    A.restore(s.As);
    state.restore(s.ss);
    ActionExecutor ae(&state,s.action_count-1);
    ae.cont.back().run(A,&ae);
    
    // push back new tasks
    TaskSet ts = s.cont.tail();
    for(auto nt = ae.get(); !nt.empty(); nt = nt.tail()) ts.push(A,nt.head());
    if(ts.empty()) { return {1,steps}; }

    // determine next actions
    ActionCollector ac(&state);
    ts.back().run(A,&ac);
    saves.push_back(Save{ts,state.save(),A.save(),ac.count()});
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

static ProverOutput prove(const Ctx &ctx, memory::Alloc &A, const ClauseIndex &cla_index, const FunOrd &fun_ord, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(cla_index,fun_ord);
  auto res = search(ctx,A,s);
  s.stats.val = s.val.stats;
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
