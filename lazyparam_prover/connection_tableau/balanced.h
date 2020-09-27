#ifndef CONNECTION_TABLEAU_BALANCED_H_
#define CONNECTION_TABLEAU_BALANCED_H_

#include "lazyparam_prover/connection_tableau/cont.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau::connection_tableau::balanced {

// [a] Task,min,max -> {[c,d]}
// [b] TaskSet,max -> [a(max),b],[b(max),a(min)]
// [c] Task,Action,max -> [b] (task set is known, but you have to push state forward)
// [d] CheckRange { min } -> []
struct _SpecCheckMin { size_t min; };
struct _SpecTask { Task task; size_t min,max; };
struct _SpecAction { Task task; Action action; size_t max; };
struct _SpecTaskSet { TaskSet task_set; size_t max; };
using SpecCheckMin = memory::Variant<0,_SpecCheckMin>;
using SpecTask = memory::Variant<1,_SpecTask>;
using SpecAction = memory::Variant<2,_SpecAction>;
using SpecTaskSet = memory::Variant<3,_SpecTaskSet>;
using Spec = memory::Coprod<SpecCheckMin,SpecTask,SpecAction,SpecTaskSet>;
template<typename FCheckMin, typename FTask, typename FAction, typename FTaskSet> auto spec_switch(
    Spec spec, FCheckMin fcheckmin, FTask ftask, FAction faction, FTaskSet ftaskset){
  switch(spec.type()) {
  case SpecCheckMin::ID: return fcheckmin(SpecCheckMin(spec));
  case SpecTask::ID: return ftask(SpecTask(spec));
  case SpecAction::ID: return faction(SpecAction(spec));
  case SpecTaskSet::ID: return ftaskset(SpecTaskSet(spec));
  default: error("unmatched case");
  }
}

using Cont = memory::List<Spec>;
template<typename Div> void spec_run(memory::Alloc &A, SearchState &state, Spec spec, Div diverge, const Cont cont) {
  static_assert(memory::has_sig<Div,void(SearchState::Save,Cont)>());
  spec_switch(spec,
    [&](SpecCheckMin scm){
      if(state.nodes_used<scm->min) return;
      diverge(state.save(),cont);
    },
    [&](SpecTask st){
      if(state.nodes_used+st->min > st->max) return;
      Cont c = cont;
      if(st->min>0) c.push(A,Spec(SpecCheckMin::alloc(A,[&](_SpecCheckMin &scm){
        scm.min = state.nodes_used+st->min;
      })));
      auto ss = state.save();
      task_iterate_actions(A,state,st->task,[&](Action a){
        diverge(ss,c.add(A,Spec(SpecAction::alloc(A,[&](_SpecAction &sa){
          sa.task = st->task;
          sa.action = a;
          sa.max = st->max;
        }))));
      });
    },
    [&](SpecAction sa){
      TaskSet ts = task_execute_action(A,state,sa->task,sa->action);
      if(state.nodes_used>sa->max) return;
      diverge(state.save(),cont.add(A,Spec(SpecTaskSet::alloc(A,[&](_SpecTaskSet &sts){
        sts.task_set = ts;
        sts.max = sa->max;
      }))));
    },
    [&](SpecTaskSet sts){
      auto ss = state.save();
      if(sts->task_set.empty()){ diverge(ss,cont); return; }
      size_t budget = sts->max-state.nodes_used;
      size_t head_budget = budget/sts->task_set.size();
      auto head = sts->task_set.head();
      auto tail = sts->task_set.tail();
      {
        Cont c = cont;
        if(!tail.empty()) c.push(A,Spec(SpecTaskSet::alloc(A,[&](_SpecTaskSet &sts2){
          sts2.task_set = tail;
          sts2.max = sts->max;
        })));
        diverge(ss,c.add(A,Spec(SpecTask::alloc(A,[&](_SpecTask &st){
          st.task = head;
          st.min = 0;
          st.max = state.nodes_used+head_budget;
        }))));
      }
      if(!tail.empty() && head_budget<budget) diverge(ss,cont
        .add(A,Spec(SpecTask::alloc(A,[&](_SpecTask &st){
          st.task = head;
          st.min = state.nodes_used+head_budget+1;
          st.max = sts->max;
        })))
        .add(A,Spec(SpecTaskSet::alloc(A,[&](_SpecTaskSet &sts2){
          sts2.task_set = tail;
          sts2.max = sts->max-(head_budget+1);
        })))
      );
    }
  );
}

// Search takes alloc as an argument to be able to return result in its memory.
alt::SearchResult search(const Ctx &ctx, memory::Alloc &A, SearchState &state, size_t size_limit) { FRAME("connection_tableau::balanced_search()");
  SCOPE("connection_tableau::balanced_search");

  struct Save {
    Cont cont;
    SearchState::Save ss;
    memory::Alloc::Save As;
  };
  vec<Save> saves;
  auto div = [&](SearchState::Save ss, Cont cont){ saves.push_back(Save{
    .cont = cont,
    .ss = ss,
    .As = A.save(),
  }); };

  div(state.save(),Cont(A,Spec(SpecTask::alloc(A,[&](_SpecTask &st){
    st.task = start_task(A);
    st.min = 0;
    st.max = size_limit;
  }))));
  
  size_t steps = 0;
  for(; saves.size(); steps++) {
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);

    const auto s = saves.back();
    saves.pop_back();
    A.restore(s.As);
    state.restore(s.ss);
    if(s.cont.empty()) return {1,steps};
    spec_run(A,state,s.cont.head(),div,s.cont.tail());
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

} // namespace connection_tableau::tableau::balanced

#endif  // CONNECTION_TABLEAU_BALANCED_H_
