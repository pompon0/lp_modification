#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"
#include "lazyparam_prover/connection_tableau/balanced.h"

namespace tableau::connection_tableau {

struct _SpecAction { Task task; Action action; };
using SpecTask = memory::Variant<0,Task>;
using SpecAction = memory::Variant<1,_SpecAction>;
using Spec = memory::Coprod<SpecTask,SpecAction>;
template<typename FTask, typename FAction> INL auto spec_switch(Spec spec, FTask ftask, FAction faction) {
  switch(spec.type()) {
  case SpecTask::ID: return ftask(SpecTask(spec));
  case SpecAction::ID: return faction(SpecAction(spec));
  default: error("unmatched case");
  }
}

// Search takes alloc as an argument to be able to return result in its memory.
INL alt::SearchResult search(const Ctx &ctx, memory::Alloc &A, SearchState &state, size_t depth_limit) { FRAME("connection_tableau::search()");
  SCOPE("connection_tableau::search");

  using Cont = memory::List<Spec>;
  struct Save {
    Cont cont;
    SearchState::Save ss;
    memory::Alloc::Save As;
  };
  vec<Save> saves;
  auto div = [&](SearchState::Save ss, Cont cont)INLL{ saves.push_back(Save{
    .cont = cont,
    .ss = ss,
    .As = A.save(),
  }); };

  div(state.save(),Cont(A,Spec(SpecTask::alloc(A,[&](Task &t)INLL { t = start_task(A); }))));
  
  size_t steps = 0;
  for(; saves.size(); steps++) {
    auto s = saves.back(); saves.pop_back();
    A.restore(s.As);
    state.restore(s.ss);
    if(s.cont.empty()) return {1,steps};
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    
    spec_switch(s.cont.head(),
      [&](SpecTask st)INLL{
        if(task_features(*st).depth>depth_limit) return;
        task_iterate_actions(A,state,*st,[&](Action a)INLL{
          div(s.ss,s.cont.tail().add(A,Spec(SpecAction::alloc(A,[&](_SpecAction &sa)INLL {
            sa.task = *st;
            sa.action = a;
          }))));
        });
      },
      [&](SpecAction sa)INLL{
        auto cont = s.cont.tail();
        for(auto ts = task_execute_action(A,state,sa->task,sa->action); !ts.empty(); ts = ts.tail()) {
          cont.push(A,Spec(SpecTask::alloc(A,[&](Task &t)INLL{ t = ts.head(); })));
        }
        div(state.save(),cont);
      }
    );
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
