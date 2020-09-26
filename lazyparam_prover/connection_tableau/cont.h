#ifndef CONNECTION_TABLEAU_CONT_H_
#define CONNECTION_TABLEAU_CONT_H_

#include "utils/ctx.h"
#include "utils/log.h"
#include "utils/types.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/function.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/alt.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

namespace tableau::connection_tableau {

struct Features {
  size_t depth;
};

struct _ {
  struct _StartFrame;
  struct _WeakFrame;
  using StartFrame = memory::Variant<0,_StartFrame>;
  using WeakFrame = memory::Variant<1,_WeakFrame>;
  // TODO: every frame/task carries an early success constraint (to catch lemmas matching after unifications).
  using Task = memory::Coprod<StartFrame,WeakFrame>;
  using TaskSet = memory::List<Task>;
  #include "lazyparam_prover/connection_tableau/frames/start.h"
  #include "lazyparam_prover/connection_tableau/frames/strong.h"
  #include "lazyparam_prover/connection_tableau/frames/weak.h"
};

using _StartFrame = _::_StartFrame;
using _WeakFrame = _::_WeakFrame;
using StartFrame = _::StartFrame;
using WeakFrame = _::WeakFrame;
using Task = _::Task;
using TaskSet = _::TaskSet;

template<typename FStartFrame, typename FWeakFrame> INL auto task_switch(
    Task task, FStartFrame fstartframe, FWeakFrame fweakframe) {
  switch(task.type()) {
  case StartFrame::ID: return fstartframe(StartFrame(task));
  case WeakFrame::ID: return fweakframe(WeakFrame(task));
  default: error("unmatched case");
  }
}

INL static inline Task start_task(memory::Alloc &A) {
  return Task(StartFrame::Builder(A).build());
}

INL static Features task_features(Task t) {
  return task_switch(t,
    [](StartFrame f){ return Features{ .depth = 0 }; },
    [](WeakFrame f){ return Features{ .depth = f->branch.false_.size() }; }
  );
};

DEBUG_ONLY(
struct Mutex {
  struct Locked {
    INL ~Locked(){ m->locked = false; }
    Mutex *m;
  };
  [[nodiscard]] INL Locked lock(){
    if(locked) error("already locked");
    locked = true;
    return {this};
  }
private:
  bool locked = false;
};)

struct Action { size_t idx; };

template<typename H> struct ActionIterator {
  ActionIterator(H _h, SearchState *_state) : h(_h), state(_state) {}
  H h;
  SearchState *state;
  size_t action_count = 0;
  DEBUG_ONLY(Mutex diverging;)

  template<typename F> [[nodiscard]] INL bool diverge(memory::Alloc &A, F f) { FRAME("ActionCollector::diverge(action_count=%)",action_count);
    static_assert(memory::has_sig<F,memory::Maybe<TaskSet>(void)>());
    DEBUG_ONLY(auto l = diverging.lock();)
    auto s = state->save();
    // TODO: add more details to Action object.
    if(f()) h(Action{action_count});
    action_count++;
    state->restore(s);
    return false;
  }
};

template<typename H> INL void task_iterate_actions(memory::Alloc &A, SearchState &state, Task t, H h) {
  static_assert(memory::has_sig<H,void(Action)>());
  auto s = state.save();
  ActionIterator<H> d(h,&state);
  task_switch(t,
    [&](StartFrame f)INLL{ f->run(A,&d); },
    [&](WeakFrame f)INLL{ f->run(A,&d); }
  );
  state.restore(s);
}

struct ActionExecutor {
  ActionExecutor(SearchState *_state, Action _action) : state(_state), action(_action) {
    FRAME("ActionExecutor(action.idx = %)",action.idx);
  } 
  DEBUG_ONLY(Mutex diverging;)
  SearchState *state;
  Action action;
  memory::Maybe<TaskSet> task_set;
  size_t action_count = 0;
  
  template<typename F> [[nodiscard]] INL bool diverge(memory::Alloc &A, F f) { FRAME("ActionCollector::diverge");
    static_assert(memory::has_sig<F,memory::Maybe<TaskSet>(void)>());
    DEBUG_ONLY(auto l = diverging.lock();)
    if(action_count++==action.idx){
      task_set = f();
      return true;
    }
    return false;
  }
};

INL static TaskSet task_execute_action(memory::Alloc &A, SearchState &state, Task task, Action action) {
  ActionExecutor d(&state,action);
  task_switch(task,
    [&](StartFrame f)INLL{ f->run(A,&d); },
    [&](WeakFrame f)INLL{ f->run(A,&d); }
  );
  DEBUG if(!d.task_set) error("action not found");
  return d.task_set.get();
}

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_CONT_H_
