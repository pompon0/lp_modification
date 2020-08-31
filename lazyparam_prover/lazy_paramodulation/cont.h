#ifndef LAZY_PARAMODULATION_CONT_H_
#define LAZY_PARAMODULATION_CONT_H_

#include "lazyparam_prover/lazy_paramodulation/split.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/memory/variant.h"
#include "lazyparam_prover/memory/lazy.h"
#include "lazyparam_prover/alt.h"

namespace tableau::lazy_paramodulation {

struct AxiomClause : Lazy<DerAndClause>::Impl {
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

  SearchState::Save save;
  SearchState *state;
  List<Frame> frames;
  bool done(){ 
    FRAME("done() :: %, frames.size() = %",frames.empty(),frames.size());
    return frames.empty();
  }

  struct Builder {
    SearchState *state;
    List<Frame> frames;
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

  [[nodiscard]] List<Cont> run(memory::Alloc &A) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    state->restore(save);
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: return start(A,StartFrame(f));
      case Frame::STRONG: return strong(A,StrongFrame(f));
      case Frame::WEAK_SET: return weak_set(A,WeakSetFrame(f));
      case Frame::WEAK: return weak(A,WeakFrame(f));
      case Frame::WEAK_UNIFY: return weak_unify(A,WeakUnifyFrame(f));
      case Frame::MIN_COST: return min_cost(A,MinCostFrame(f));
      case Frame::REDUCTION: return reduction(A,ReductionFrame(f));
      case Frame::LAZY_WEAK_CONNECTION: return lazy_weak_connection(A,LazyWeakConnectionFrame(f));
      case Frame::LAZY_PRE_WEAK_CONNECTION: return lazy_pre_weak_connection(A,LazyPreWeakConnectionFrame(f));
      case Frame::LAZY_STRONG_CONNECTION: return lazy_strong_connection(A,LazyStrongConnectionFrame(f));
      case Frame::LAZY_PRE_STRONG_CONNECTION: return lazy_pre_strong_connection(A,LazyPreStrongConnectionFrame(f));
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
