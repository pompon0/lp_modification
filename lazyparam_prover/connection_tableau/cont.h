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

namespace tableau::connection_tableau {

struct Features {
  size_t depth;
};

struct _ {

struct Frame;
using Task = Frame;
using TaskSet = memory::List<Task>;

#include "lazyparam_prover/connection_tableau/frames/start.h"
#include "lazyparam_prover/connection_tableau/frames/strong.h"
#include "lazyparam_prover/connection_tableau/frames/weak.h"

// TODO: every frame/task carries an early success constraint (to catch lemmas matching after unifications).
struct Frame {
private:
  enum Type { START, WEAK };
  INL Type type() const { return Type(*LType::at(ptr)); }
  
  using LType = memory::Lens<size_t,0>;
  enum { SIZE = LType::END };
  uint8_t *ptr;
  INL explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}
public:
  using Start = memory::Variant<Frame,START,StartFrame>;
  using Weak = memory::Variant<Frame,WEAK,WeakFrame>;
  friend Start;
  friend Weak;
  template<typename DState> INL void run(memory::Alloc &A, DState *d) const { FRAME("run");
    switch(type()) {
      case Frame::START: Start(*this)->run(A,d); return;
      case Frame::WEAK: Weak(*this)->run(A,d); return;
      default: error("frame.type() = %",type());
    }
  }
  Features features() const {
    switch(type()) {
      case Frame::START: return Start(*this)->features();
      case Frame::WEAK: return Weak(*this)->features();
      default: error("frame.type() = %",type());
    }
  }
};

};

using Task = _::Task;
using TaskSet = _::TaskSet;

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_CONT_H_
