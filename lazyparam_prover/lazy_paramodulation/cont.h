#ifndef LAZY_PARAMODULATION_CONT_H_
#define LAZY_PARAMODULATION_CONT_H_

#include "lazyparam_prover/lazy_paramodulation/split.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/memory/lazy.h"
#include "lazyparam_prover/alt.h"

namespace tableau::lazy_paramodulation {

struct AxiomClause : memory::Lazy<DerAndClause>::Impl {
  AndClause cla;
  INL AxiomClause(AndClause _cla) : cla(_cla) {}
  DerAndClause get(memory::Alloc &A) const {
    DerAndClause::Builder b(A);
    b.derived = cla;
    b.sources.push_back(cla);
    return b.build(A);
  }
};

struct Cont { 
  using State = SearchState;
 
  struct _StartFrame;
  struct _StrongFrame;
  struct _WeakSetFrame;
  struct _WeakFrame;
  struct _WeakUnifyFrame;
  struct _MinCostFrame;

  struct _ReductionFrame;
  struct _LazyWeakConnectionFrame;
  struct _LazyPreWeakConnectionFrame;
  struct _LazyStrongConnectionFrame;
  struct _LazyPreStrongConnectionFrame;

  using StartFrame = memory::Variant<0,_StartFrame>;
  using StrongFrame = memory::Variant<1,_StrongFrame>;
  using WeakSetFrame = memory::Variant<2,_WeakSetFrame>;
  using WeakFrame = memory::Variant<3,_WeakFrame>;
  using WeakUnifyFrame = memory::Variant<4,_WeakUnifyFrame>;
  using MinCostFrame = memory::Variant<5,_MinCostFrame>;
  ////////////////////////////////////
  using ReductionFrame = memory::Variant<6,_ReductionFrame>;
  using LazyWeakConnectionFrame = memory::Variant<7,_LazyWeakConnectionFrame>;
  using LazyPreWeakConnectionFrame = memory::Variant<8,_LazyPreWeakConnectionFrame>;
  using LazyStrongConnectionFrame = memory::Variant<9,_LazyStrongConnectionFrame>;
  using LazyPreStrongConnectionFrame = memory::Variant<10,_LazyPreStrongConnectionFrame>;
  using Frame = memory::Coprod<
    StartFrame,
    StrongFrame,
    WeakSetFrame,
    WeakFrame,
    WeakUnifyFrame,
    MinCostFrame,
    ReductionFrame,
    LazyWeakConnectionFrame,
    LazyPreWeakConnectionFrame,
    LazyStrongConnectionFrame,
    LazyPreStrongConnectionFrame
  >;

  SearchState::Save save;
  SearchState *state;
  memory::List<Frame> frames;
  bool done(){ 
    FRAME("done() :: %, frames.size() = %",frames.empty(),frames.size());
    return frames.empty();
  }

  struct Builder {
    SearchState *state;
    memory::List<Frame> frames;
  public:
    [[nodiscard]] Builder add(memory::Alloc &A, Frame f){ return Builder{state,frames.add(A,f)}; }
    [[nodiscard]] Cont build() {
      return Cont {
        .save = state->save(),
        .state = state,
        .frames = frames,
      };
    }
  };
  
  [[nodiscard]] Builder builder() const { return Builder{state,frames.tail()}; }

  [[nodiscard]] memory::List<Cont> run(memory::Alloc &A) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    state->restore(save);
    auto f = frames.head();
    switch(f.type()) {
      case StartFrame::ID: return start(A,StartFrame(f));
      case StrongFrame::ID: return strong(A,StrongFrame(f));
      case WeakSetFrame::ID: return weak_set(A,WeakSetFrame(f));
      case WeakFrame::ID: return weak(A,WeakFrame(f));
      case WeakUnifyFrame::ID: return weak_unify(A,WeakUnifyFrame(f));
      case MinCostFrame::ID: return min_cost(A,MinCostFrame(f));
      case ReductionFrame::ID: return reduction(A,ReductionFrame(f));
      case LazyWeakConnectionFrame::ID: return lazy_weak_connection(A,LazyWeakConnectionFrame(f));
      case LazyPreWeakConnectionFrame::ID: return lazy_pre_weak_connection(A,LazyPreWeakConnectionFrame(f));
      case LazyStrongConnectionFrame::ID: return lazy_strong_connection(A,LazyStrongConnectionFrame(f));
      case LazyPreStrongConnectionFrame::ID: return lazy_pre_strong_connection(A,LazyPreStrongConnectionFrame(f));
      default: error("f.type() = %",f.type());
    }
  }

#include "lazyparam_prover/lazy_paramodulation/frames/start.h"
#include "lazyparam_prover/lazy_paramodulation/frames/strong.h"
#include "lazyparam_prover/lazy_paramodulation/frames/weak_set.h"
#include "lazyparam_prover/lazy_paramodulation/frames/weak.h"
#include "lazyparam_prover/lazy_paramodulation/frames/weak_unify.h"
#include "lazyparam_prover/lazy_paramodulation/frames/min_cost.h"
#include "lazyparam_prover/lazy_paramodulation/frames/reduction.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_weak_connection.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_pre_weak_connection.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_strong_connection.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_pre_strong_connection.h"

};

} // namespace lazy_paramodulation::tableau

#endif  // LAZY_PARAMODULATION_CONT_H_
