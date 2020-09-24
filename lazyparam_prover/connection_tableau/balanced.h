#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"

namespace tableau::connection_tableau {

// Search takes alloc as an argument to be able to return result in its memory.
alt::SearchResult balanced_search(const Ctx &ctx, memory::Alloc &A, SearchState &state, size_t size_limit) { FRAME("connection_tableau::balanced_search()");
  SCOPE("connection_tableau::balanced_search");
  struct _SizedTask { Task task; size_t min,max; };
  struct _SizedTaskSet { TaskSet task_set; size_t min,max; };
  struct _SizedActions { Task task; memory::List<memory::Maybe<TaskSet>> actions; size_t min,max; };
  using SizedTask = memory::Variant<0,_SizedTask>;
  using SizedTaskSet = memory::Variant<1,_SizedTaskSet>;
  using SizedActions = memory::variant<2,_SizedActions>;
  using SuperTask = memory::Coprod<SizedTask,SizedTaskSet,SizedActions>;
  struct Save {
    memory::List<SuperTask> cont;
    SearchState::Save ss;
    memory::Alloc::Save As;
  };

  auto st = SizedTask::alloc(A);
  st->task = start_task(A);
  st->min = 0;
  st->max = size_max; 
  memory::List<SuperTask> cont(A,SuperTask(st));
  vec<Save> saves{
    .cont = cont,
    .ss = state.save(),
    .As = A.save(),
  }};
  
  size_t steps = 0;
  for(; saves.size(); steps++) {
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);

    // select action
    auto s = saves.back();
    A.restore(s.As);
    state.restore(s.ss);
    if(s.cont.empty()) return {1,steps};
    
    auto st = s.cont.head();
    switch(st.type()) {
      case SizedTask::ID: {
        SizedTask st(st);
        // determine next actions
        ActionCollector ac(&state);
        run(A,st->task,&ac);
        auto sa = SizedActions::alloc(A);
        sa->task = st->task;
        sa->actions = ac.actions;
        sa->min = st->min;
        sa->max = st->max;
        saves.back().cont = s.cont.tail().add(A,SuperTask(sa));
        break;
      }
      case SizedTaskSet::ID: {
        // TODO: set decomposition logic
        break;
      }
      case SizedActions::ID: {
        SizedActions st(st);
        while(!st->actions.empty() && !st->actions.head()) st->ctions = st->actions.tail();
        if(st->actions.empty()){ saves.pop_back(); continue; }
        auto a = st->actions.head();
        st->actions = st->actions.tail();
        
        // execute action
        A.restore(s.As);
        state.restore(s.ss);
        ActionExecutor ae(&state,st->actions.size());
        run(A,st->task,&ae);
       
        // push the resulting sized task set
        auto sts = SizedTaskSet::alloc(A);
        sts->task_set = ae.get();
        sts->min = st->min;
        sts->max = st->max;
        Save s2;
        s2.cont = s.cont.tail().add(A,SuperTask(sts));
        s2.ss = state.save();
        s2.As = A.save();
        saves.push_back(s2);
        break;
      }
    }

   
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
