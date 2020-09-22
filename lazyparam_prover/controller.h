#ifndef CONTROLLER_H_
#define CONTROLLER_H_

#include <memory>
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/connection_tableau/tableau.h"
#include "tool/bin/wrapper.h"

namespace controller {

class Problem {
public:
  using Ptr = std::shared_ptr<const Problem>;
  static Ptr New(const str &tptp_fof) {
    auto file = tptp_to_proto(tptp_cnf(tptp_fof));
    auto p = own(new Problem());
    p->A = make<memory::Alloc>();
    p->idx = make<tableau::ClauseIndex>(tableau::ParseCtx().parse_orForm(*p->A,file));
    return p;
  }
  friend class Prover;
private:
  Problem() = default;
  ptr<memory::Alloc> A;
  ptr<tableau::ClauseIndex> idx;

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

class Prover {
  using Cont = tableau::connection_tableau::Cont;
public:
  struct Action {
    bool done() const { return cont.done(); }
    tableau::connection_tableau::Cont cont;
    memory::Alloc::Save A;
  };

  static ptr<Prover> New(Problem::Ptr problem) {
    auto p = own(new Prover());
    p->A = make<memory::Alloc>();
    p->state = make<tableau::SearchState>(*problem->idx,FunOrd());
    auto cont = tableau::connection_tableau::start_cont(*p->A,*p->state,LIMIT);
    p->start = own(new Action{cont,p->A->save()});
    return p; 
  }

  Action reset() { return *start; }

  // TODO: you cannot return to arbitrary action, try sth else.
  vec<Action> run(Action a) {
    A->restore(a.A);
    memory::List<Cont> cs = a.cont.run(*A);
    auto As = A->save();
    vec<Action> as;
    for(; !cs.empty(); cs = cs.tail()) {
      as.push_back(Action{cs.head(),As});
    }
    return as;
  }

private:
  enum { LIMIT = 1000000 };
  Prover() = default;
  ptr<memory::Alloc> A;
  ptr<tableau::SearchState> state; 
  ptr<Action> start;
};

tableau::alt::SearchResult search(const Ctx &ctx, Prover &p) { FRAME("controller::search");
  SCOPE("controller::search");
  vec<Prover::Action> as{p.reset()};
  size_t steps = 0;
  for(;as.size(); steps++) {
    Prover::Action a = as.back();
    as.pop_back();
    if(a.done()) return {1,steps};
    if(steps%100==0 && ctx.done()) return {0,steps};
    DEBUG if(steps%1000==0) info("steps = %",steps);
    for(auto x : p.run(a)) as.push_back(x);
  }
  DEBUG info("steps = %",steps);
  return {0,steps};
}

} // namespace controller

#endif // CONTROLLER_H_
