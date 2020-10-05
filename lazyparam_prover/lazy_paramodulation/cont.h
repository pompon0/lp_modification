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

struct _ {
using State = SearchState;
#include "lazyparam_prover/lazy_paramodulation/frames/start.h"
#include "lazyparam_prover/lazy_paramodulation/frames/strong.h"
#include "lazyparam_prover/lazy_paramodulation/frames/weak.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_weak_connection.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_pre_weak_connection.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_strong_connection.h"
#include "lazyparam_prover/lazy_paramodulation/frames/lazy_pre_strong_connection.h"
};

template<typename Div> INL static inline void start_task(Div *d) {
  _::_StartFrame{}.run(d);
}

} // namespace lazy_paramodulation::tableau

#endif  // LAZY_PARAMODULATION_CONT_H_
