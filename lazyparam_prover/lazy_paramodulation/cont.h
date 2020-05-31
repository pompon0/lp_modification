#ifndef LAZY_PARAMODULATION_CONT_H_

#include "lazyparam_prover/lazy_paramodulation/split.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/memory/variant.h"
#include "lazyparam_prover/alt.h"

namespace tableau::lazy_paramodulation {

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

  struct Frame {
  public:
    enum Type {
      START,
      STRONG,
      WEAK_SET,
      WEAK,
      WEAK_UNIFY,
      MIN_COST,
      //////////
      REDUCTION,
      LAZY_WEAK_CONNECTION,
      LAZY_PRE_WEAK_CONNECTION,
      LAZY_STRONG_CONNECTION,
      LAZY_PRE_STRONG_CONNECTION,
    };
    Type type() const { return Type(*LType::at(ptr)); }
  private:
    using LType = Lens<size_t,0>;
    enum { SIZE = LType::END };
    uint8_t *ptr;
    explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

    friend Variant<Frame,START,_StartFrame>;
    friend Variant<Frame,STRONG,_StrongFrame>;
    friend Variant<Frame,WEAK_SET,_WeakSetFrame>;
    friend Variant<Frame,WEAK,_WeakFrame>;
    friend Variant<Frame,WEAK_UNIFY,_WeakUnifyFrame>;
    friend Variant<Frame,MIN_COST,_MinCostFrame>;
    ////////////////////////////////////
    friend Variant<Frame,REDUCTION,_ReductionFrame>;
    friend Variant<Frame,LAZY_WEAK_CONNECTION,_LazyWeakConnectionFrame>;
    friend Variant<Frame,LAZY_PRE_WEAK_CONNECTION,_LazyPreWeakConnectionFrame>;
    friend Variant<Frame,LAZY_STRONG_CONNECTION,_LazyStrongConnectionFrame>;
    friend Variant<Frame,LAZY_PRE_STRONG_CONNECTION,_LazyPreStrongConnectionFrame>;
  };
  
  List<Frame> frames;
  bool done(){ return frames.empty(); }

  template<typename Alts> void run(State &state, Alts alts) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: start(state,StartFrame(f),alts); break;
      case Frame::STRONG: strong(state,StrongFrame(f),alts); break;
      case Frame::WEAK_SET: weak_set(state,WeakSetFrame(f),alts); break;
      case Frame::WEAK: weak(state,WeakFrame(f),alts); break;
      case Frame::WEAK_UNIFY: weak_unify(state,WeakUnifyFrame(f),alts); break;
      case Frame::MIN_COST: min_cost(state,MinCostFrame(f),alts); break;
      case Frame::REDUCTION: reduction(state,ReductionFrame(f),alts); break;
      case Frame::LAZY_WEAK_CONNECTION: lazy_weak_connection(state,LazyWeakConnectionFrame(f),alts); break;
      case Frame::LAZY_PRE_WEAK_CONNECTION: lazy_pre_weak_connection(state,LazyPreWeakConnectionFrame(f),alts); break;
      case Frame::LAZY_STRONG_CONNECTION: lazy_strong_connection(state,LazyStrongConnectionFrame(f),alts); break;
      case Frame::LAZY_PRE_STRONG_CONNECTION: lazy_pre_strong_connection(state,LazyPreStrongConnectionFrame(f),alts); break;
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
