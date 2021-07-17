#ifndef ENGINE_BALANCED_H_
#define ENGINE_BALANCED_H_

#include "lazyparam_prover/memory/layout.h"
#include "lazyparam_prover/engine/engine.h"

namespace tableau::engine::balanced {

struct Proxy;

// [a] Task,min,max -> {[b,d]}
// [b] TaskSet,max -> [a(max),b],[b(max),a(min)]
// [d] CheckRange { min } -> []
using Task = memory::function<void(Proxy*)>;
using TaskSet = memory::List<Task>;
// TODO: indicate that min in checkmin is absolute, while in task it is relative
struct _SpecCheckMin { size_t min; };
struct _SpecTask { Task task; size_t min,max; };
struct _SpecTaskSet { TaskSet task_set; size_t max; };
using SpecCheckMin = memory::Variant<0,_SpecCheckMin>;
using SpecTask = memory::Variant<1,_SpecTask>;
using SpecTaskSet = memory::Variant<2,_SpecTaskSet>;
using Spec = memory::Coprod<SpecCheckMin,SpecTask,SpecTaskSet>;
using Cont = memory::List<Spec>;
template<typename FCheckMin, typename FTask, typename FTaskSet> auto spec_switch(
    Spec spec, FCheckMin fcheckmin, FTask ftask, FTaskSet ftaskset){
  switch(spec.type()) {
  case SpecCheckMin::ID: return fcheckmin(SpecCheckMin(spec));
  case SpecTask::ID: return ftask(SpecTask(spec));
  case SpecTaskSet::ID: return ftaskset(SpecTaskSet(spec));
  default: error("unmatched case");
  }
}

struct Div {
  template<typename F> INL Div(memory::Alloc &_A, SearchState *_state, bool _cut, size_t size_limit, F f)
      : A(_A), state(_state), cut(_cut) {
    save(Cont(A,Spec(SpecTask::alloc(A,[&](_SpecTask &st)INLL{
      st.min = 0;
      st.max = size_limit;
      st.task = Task(A,f);
    }))));
  }
  memory::Alloc &A;
  SearchState *state;
  bool cut;

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

  //////////////////////////////

  INL bool step();
};

struct Proxy {
  struct Alt {
    Proxy *p;
    memory::List<Task> new_tasks;
    template<typename F> INL void task(memory::Maybe<Atom> goal, F f){ new_tasks.push(p->A,Task(p->A,f)); }

    INL void feature_branch(Branch) {}
    INL void feature_mcts_node(bool){}
  };

  memory::Alloc &A;
  SearchState *state;
  size_t size_limit_;
  INL size_t size_limit(){ return size_limit_; }
  
  template<typename F> INL void alt(F f) {
    Alt alt{this};
    f(&alt);
    div->save(cont.add(A,Spec(SpecTaskSet::alloc(A,[&](_SpecTaskSet &sts)INLL{
      sts.max = size_limit_;
      sts.task_set = alt.new_tasks;
    }))));
  }
  
  Div *div;
  Cont cont;
};

INL bool Div::step() {
  PROF_CYCLES("engine::balanced::Div::step");
  state->stats.steps++;
  auto s = saves.back(); saves.pop_back();
  A.restore(s.As);
  state->restore(s.ss);
  if(s.cont.empty()){ return true; }
  spec_switch(s.cont.head(),
    [&](SpecCheckMin scm)INLL{
      if(state->nodes_used<scm->min) return;
      save(s.cont.tail());
    },
    [&](SpecTask st)INLL{
      if(state->nodes_used+st->min > st->max) return;
      Cont c = s.cont.tail();
      if(st->min>0) c.push(A,Spec(SpecCheckMin::alloc(A,[&](_SpecCheckMin &scm)INLL{
        scm.min = state->nodes_used+st->min;
      })));
      Proxy P{
        .A = A,
        .state = state,
        .size_limit_ = st->max,
        .div = this,
        .cont = c,
      };
      {
        PROF_CYCLES("engine::balanced::Div::step_SpecTask_task");
        st->task(&P);
      }
    },
    [&](SpecTaskSet sts)INLL{
      PROF_CYCLES("engine::balanced::Div::step_SpecTaskSet");
      if(sts->task_set.empty()){ save(s.cont.tail()); return; }
      size_t budget = sts->max-state->nodes_used;
      size_t head_budget = budget/sts->task_set.size();
      auto head = sts->task_set.head();
      auto tail = sts->task_set.tail();
      {
        Cont c = s.cont.tail();
        if(!tail.empty()) c.push(A,Spec(SpecTaskSet::alloc(A,[&](_SpecTaskSet &sts2)INLL{
          sts2.task_set = tail;
          sts2.max = sts->max;
        })));
        save(c.add(A,Spec(SpecTask::alloc(A,[&](_SpecTask &st)INLL{
          st.task = head;
          st.min = 0;
          st.max = state->nodes_used+head_budget;
        }))));
      }
      if(!tail.empty() && head_budget<budget) save(s.cont.tail()
        .add(A,Spec(SpecTask::alloc(A,[&](_SpecTask &st)INLL{
          st.task = head;
          st.min = head_budget+1;
          st.max = sts->max;
        })))
        .add(A,Spec(SpecTaskSet::alloc(A,[&](_SpecTaskSet &sts2)INLL{
          sts2.task_set = tail;
          sts2.max = sts->max-(head_budget+1);
        })))
      );
    }
  );
  return false;
}

// Search takes alloc as an argument to be able to return result in its memory.
// TODO: generalize search and test it to find random test trees - to make sure that is doesn't skip any part of search space.
template<typename Cont> INL bool search(const Ctx &ctx, memory::Alloc &A, SearchState &state, bool cut, size_t size_limit) { FRAME("connection_tableau::balanced_search()");
  PROF_CYCLES("engine::balanced::search");
  Div d(A,&state,cut,size_limit,[](Proxy *p){ Cont::start(p); });
  size_t steps = 0;
  for(; d.saves.size(); steps++) {
    if(d.step()) return true;
    if(steps%100==0 && ctx.done()) return false;
    DEBUG if(steps%1000==0) info("steps = %",steps);
  }
  DEBUG info("steps = %",steps);
  return false;
}

template<typename Cont> static ProverOutput schedule(
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

} // namespace tableau::engine::balanced

#endif  // ENGINE_BALANCED_H_
