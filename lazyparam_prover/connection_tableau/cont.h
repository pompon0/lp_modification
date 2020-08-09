#ifndef CONNECTION_TABLEAU_CONT_H_
#define CONNECTION_TABLEAU_CONT_H_

#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/variant.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/alt.h"
#include "lazyparam_prover/ctx.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

namespace tableau::connection_tableau {

struct Cont { 
  using State = SearchState;
 
  struct _StartFrame;
  struct _StrongFrame;
  struct _WeakConnectionsFrame;
  struct _WeakSetFrame;
  struct _WeakFrame;
  struct _WeakUnifyFrame;
  struct _MinCostFrame;

  struct Frame {
  public:
    enum Type { START, STRONG, WEAK_CONNECTIONS, WEAK_SET, WEAK, WEAK_UNIFY, MIN_COST };
    Type type() const { return Type(*LType::at(ptr)); }
  private:
    using LType = Lens<size_t,0>;
    enum { SIZE = LType::END };
    uint8_t *ptr;
    explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

    friend Variant<Frame,START,_StartFrame>;
    friend Variant<Frame,STRONG,_StrongFrame>;
    friend Variant<Frame,WEAK_CONNECTIONS,_WeakConnectionsFrame>;
    friend Variant<Frame,WEAK_SET,_WeakSetFrame>;
    friend Variant<Frame,WEAK,_WeakFrame>;
    friend Variant<Frame,WEAK_UNIFY,_WeakUnifyFrame>;
    friend Variant<Frame,MIN_COST,_MinCostFrame>;
  };
 
  SearchState::Save save;
  SearchState *state;
  List<Frame> frames;
  bool done(){ return frames.empty(); }

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
  
  [[nodiscard]] Builder builder() const { return Builder{state,frames}; }

  [[nodiscard]] List<Cont> run(memory::Alloc &A) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    state->restore(save);
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: return start(A,StartFrame(f));
      case Frame::STRONG: return strong(A,StrongFrame(f));
      case Frame::WEAK_CONNECTIONS: return weak_connections(A,WeakConnectionsFrame(f));
      case Frame::WEAK_SET: return weak_set(A,WeakSetFrame(f));
      case Frame::WEAK: return weak(A,WeakFrame(f));
      case Frame::WEAK_UNIFY: return weak_unify(A,WeakUnifyFrame(f));
      case Frame::MIN_COST: return min_cost(A,MinCostFrame(f));
      default: error("f.type() = %",f.type());
    }
  }
#include "lazyparam_prover/connection_tableau/frames/start.h"
#include "lazyparam_prover/connection_tableau/frames/strong.h"
#include "lazyparam_prover/connection_tableau/frames/weak_connections.h"
#include "lazyparam_prover/connection_tableau/frames/weak_set.h"
#include "lazyparam_prover/connection_tableau/frames/weak.h"
#include "lazyparam_prover/connection_tableau/frames/weak_unify.h"
#include "lazyparam_prover/connection_tableau/frames/min_cost.h"
};

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_CONT_H_
