#ifndef CONTROLLER_PROBLEM_H_
#define CONTROLLER_PROBLEM_H_

#include <memory>
#include "lazyparam_prover/controller/features.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/lpmod.h"
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
  static Ptr New(const str &tptp_fof) {
    auto file = tptp_to_proto(tptp_cnf(tptp_fof));
    auto p = own(new Problem());
    p->A = make<memory::Alloc>();
    auto [f,idx] = tableau::ProtoToSyntax::orForm(*p->A,file);
    p->idx = make<tableau::ClauseIndex>(tableau::lpmod::conv(*p->A,f));
    p->node_idx = make<tool::node::Index>(idx);
    return p;
  }
  friend class RawProver;
private:
  Problem() = default;
  ptr<memory::Alloc> A;
  ptr<const tableau::ClauseIndex> idx;
  ptr<const tool::node::Index> node_idx;

  static str tptp_cnf(const str &tptp_fof) {
    tool::Request req;
    req.mutable_fof_to_cnf()->set_tptp_fof(tptp_fof);
    return tool::bin::wrapper(Ctx::background(),req).fof_to_cnf().tptp_cnf();
  }
  static tptp::File tptp_to_proto(const str &tptp_cnf) {
    tool::Request req;
    req.mutable_tptp_to_proto()->set_tptp(tptp_cnf);
    req.mutable_tptp_to_proto()->set_lang(tptp::Input::CNF);
    return tool::bin::wrapper(Ctx::background(),req).tptp_to_proto().file();
  }
};

}  // namespace controller

#endif  // CONTROLLER_PROBLEM_H_
