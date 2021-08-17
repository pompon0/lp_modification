#ifndef CONTROLLER_PROBLEM_H_
#define CONTROLLER_PROBLEM_H_

#include <memory>
#include "lazyparam_prover/controller/features.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/lpmod.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/search_state.h"
#include "utils/types.h"
#include "tool/bin/wrapper.h"
#include "tool/node.h"

// TODO: apart running ML to choose the right alternative to find the answer,
//   run ML to minimize the search space by an appropriate choice of task order
namespace controller {

class Problem {
public:
  using Ptr = std::shared_ptr<const Problem>;
  static Ptr New(const tptp::File &file) {
    auto p = own(new Problem());
    p->A = make<memory::Alloc>();
    auto [f,idx] = tableau::ProtoToSyntax::orForm(*p->A,file);
    if(tableau::has_equality(f)) f = tableau::lpmod::conv(*p->A,f);
    p->idx = make<tableau::ClauseIndex>(f);
    p->node_idx = make<tool::node::Index>(idx);
    return p;
  }
  friend class RawProver;
private:
  Problem() = default;
  ptr<memory::Alloc> A;
  ptr<const tableau::ClauseIndex> idx;
  ptr<const tool::node::Index> node_idx;
};

}  // namespace controller

#endif  // CONTROLLER_PROBLEM_H_
