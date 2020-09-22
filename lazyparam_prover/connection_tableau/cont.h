#ifndef CONNECTION_TABLEAU_CONT_H_
#define CONNECTION_TABLEAU_CONT_H_

#include "lazyparam_prover/search_state.h"
#include "utils/types.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/variant.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "utils/log.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/alt.h"
#include "utils/ctx.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

#include "lazyparam_prover/connection_tableau/frames/start.h"
#include "lazyparam_prover/connection_tableau/frames/weak.h"

namespace tableau::connection_tableau {

// TODO: redefine Continuations to be task sets:
//   task.run() produces an alternative of task sets, which replace the original task
//   we have to ensure an invariant that for every task choice strategy, there exists alternative selection strategy, which gets us to a proof.
// TODO: task.normalize() produces a Maybe<Alt{task set}>
//   it can progress as long as there is no choice to be made
//   it is supposed to allow for checking early exit conditions independently from what is currently done.
// TODO: every frame/task carries an early success constraint (to catch lemmas matching after unifications).
struct Frame {
public:
  template<typename DState> INL void run(memory::Alloc &A, DState *d) const { FRAME("run");
    switch(frame.type()) {
      case Frame::START: StartFrame(frame)->run(A,d); return;
      case Frame::WEAK: WeakFrame(frame)->run(A,d); return;
      default: error("frame.type() = %",frame.type());
    }
  }
private:
  enum Type { START, WEAK };
  INL Type type() const { return Type(*LType::at(ptr)); }
  using Start = memory::Variant<Frame,START,StartFrame>;
  using Weak = memory::Variant<Frame,WEAK,WeakFrame>;
  
  using LType = memory::Lens<size_t,0>;
  enum { SIZE = LType::END };
  uint8_t *ptr;
  INL explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

  friend Start;
  friend Weak;
};

using Task = Frame;
using TaskSet = memory::List<Task>;

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_CONT_H_
