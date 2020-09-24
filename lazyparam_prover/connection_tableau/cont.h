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
using Frame = memory::Coprod<StartFrame,WeakFrame>;

using Task = Frame;
using TaskSet = memory::List<Task>;

#include "lazyparam_prover/connection_tableau/frames/start.h"
#include "lazyparam_prover/connection_tableau/frames/strong.h"
#include "lazyparam_prover/connection_tableau/frames/weak.h"

};

using StartFrame = _::StartFrame;
using WeakFrame = _::WeakFrame;
using Frame = _::Frame;
using Task = _::Task;
using TaskSet = _::TaskSet;

template<typename DState> INL static void run(memory::Alloc &A, Frame f, DState *d) { FRAME("run");
  switch(f.type()) {
    case StartFrame::ID: StartFrame(f)->run(A,d); return;
    case WeakFrame::ID: WeakFrame(f)->run(A,d); return;
    default: error("frame.type() = %",f.type());
  }
}

INL static Features features(Frame f) {
  switch(f.type()) {
    case StartFrame::ID: return StartFrame(f)->features();
    case WeakFrame::ID: return WeakFrame(f)->features();
    default: error("frame.type() = %",f.type());
  }
};

DEBUG_ONLY(
struct Mutex {
  struct Locked {
    ~Locked(){ m->locked = false; }
    Mutex *m;
  };
  [[nodiscard]] Locked lock(){
    if(locked) error("already locked");
    locked = true;
    return {this};
  }
private:
  bool locked = false;
};)

struct ActionCollector {
  ActionCollector(SearchState *_state) : state(_state) {}
  SearchState *state;
  // std::function uses heap allocation, we should avoid it.
  template<typename F> [[nodiscard]] INL bool diverge(memory::Alloc &A, F f) { FRAME("ActionCollector::diverge");
    static_assert(memory::has_sig<F,memory::Maybe<TaskSet>(void)>());
    DEBUG_ONLY(auto l = diverging.lock();)
    auto s = state->save();
    actions.push(A,f());
    state->restore(s);
    return false;
  }
  memory::List<memory::Maybe<TaskSet>> actions;
private:
  DEBUG_ONLY(Mutex diverging;)
};

struct ActionExecutor {
  ActionExecutor(SearchState *_state, int _skip_count) : state(_state), skip_count(_skip_count) {}
  SearchState *state;
  template<typename F> [[nodiscard]] INL bool diverge(memory::Alloc &A, F f) { FRAME("ActionExecutor::diverge");
    static_assert(memory::has_sig<F,memory::Maybe<TaskSet>(void)>());
    DEBUG_ONLY(auto l = diverging.lock();)
    if(skip_count--) return false;
    if(auto mts = f()) {
      task_set = mts.get();
    } else {
      error("task set not found");
    }
    return true;
  }
  TaskSet get() {
    DEBUG if(skip_count!=-1) error("action not found");
    return task_set;
  }
private:
  DEBUG_ONLY(Mutex diverging;)
  int skip_count;
  TaskSet task_set;
};

INL static inline Task start_task(memory::Alloc &A) {
  return Task(_::StartFrame::alloc(A));
}

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_CONT_H_
