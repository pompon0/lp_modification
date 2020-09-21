#ifndef CONNECTION_TABLEAU_TABLEAU_H_
#define CONNECTION_TABLEAU_TABLEAU_H_

#include "lazyparam_prover/connection_tableau/cont.h"

namespace tableau::connection_tableau {

static Cont start_cont(memory::Alloc &A, SearchState &state, size_t limit) {
  Cont::StartFrame::Builder b(A);
  b->nodes_limit = limit;
  return Cont{
    .save = state.save(),
    .state = &state,
    .frames = memory::List<Cont::Frame>(A,Cont::Frame(b.build())),
  };
}

static ProverOutput prove(const Ctx &ctx, memory::Alloc &A, const ClauseIndex &cla_index, const FunOrd &fun_ord, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(cla_index,fun_ord);
  auto res = alt::search(ctx,A,start_cont(A,s,limit));
  s.stats.val = s.val.stats;
  return {
    res.cont_count,
    limit,
    s.val.get_valuation(),
    res.found ? s.get_proof(A) : 0,
    s.stats,
  };
}

static ProverOutput prove_loop(const Ctx &ctx, memory::Alloc &A, OrForm form, const FunOrd &fun_ord) { FRAME("prove_loop()");
  SCOPE("prove_loop"); 
  Stats stats;
  size_t cont_count = 0;
  size_t limit = 0;
  //info("ClauseIndex begin");
  ClauseIndex idx(form);
  //info("ClauseIndex end");
  for(;!ctx.done();) {
    limit++; // avoid incrementing limit before context check
    DEBUG info("limit = %",limit);
    ProverOutput out = prove(ctx,A,idx,fun_ord,limit);
    out.cont_count += cont_count;
    out.stats += stats;
    if(out.proof) {
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*out.proof));
      return out;
    }
    stats = out.stats;
    cont_count = out.cont_count;
    //std::cerr << "expands[" << limit << "]: " << profile.scopes["expand"].count << std::endl;
  }
  DEBUG info("FAILURE");
  ProverOutput out;
  out.cont_count = cont_count;
  out.cost = limit;
  out.stats = stats;
  return out; 
}

} // namespace connection_tableau::tableau

#endif  // CONNECTION_TABLEAU_TABLEAU_H_
