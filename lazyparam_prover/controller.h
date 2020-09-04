#ifndef CONTROLLER_H_
#define CONTROLLER_H_

#include <memory>
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/parse.h"
#include "tool/bin/wrapper.h"

namespace controller {

class Problem {
public:
  using Ptr = std::shared_ptr<const Problem>;
  static Ptr New(const str &tptp_fof) {
    auto A = make<memory::Alloc>();
    auto file = tptp_to_proto(tptp_cnf(tptp_fof));
    tableau::OrForm form(tableau::ParseCtx().parse_orForm(*A,file));
    return own(new Problem(std::move(A),make<tableau::ClauseIndex>(form)));
  }
private:
  ptr<memory::Alloc> A;
  ptr<tableau::ClauseIndex> idx;
  Problem(ptr<memory::Alloc> _A, ptr<tableau::ClauseIndex> _idx) : A(std::move(_A)), idx(std::move(_idx)) {}

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

} // namespace controller

#endif // CONTROLLER_H_
