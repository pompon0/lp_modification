#ifndef LAZY_PARAMODULATION_PROVE_H_
#define LAZY_PARAMODULATION_PROVE_H_

#include "lazyparam_prover/lazy_paramodulation/cont.h"

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/variant.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/lpo.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/alt.h"
#include "lazyparam_prover/ctx.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

namespace tableau::lazy_paramodulation {

ProverOutput prove(const Ctx &ctx, const ClauseIndex &cla_index, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(cla_index);
  Cont::StartFrame::Builder b;
  b->nodes_limit = limit;
  auto res = alt::search(ctx,s,Cont{List<Cont::Frame>(Cont::Frame(b.build()))});
  s.stats.val = s.val.stats;
  return {
    res.cont_count,
    limit,
    s.val.get_valuation(),
    res.found ? s.get_proof() : 0,
    s.stats,
  };
}

ProverOutput prove_loop(const Ctx &ctx, OrForm form) { FRAME("prove_loop()");
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
    ProverOutput out = prove(ctx,idx,limit);
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

} // namespace lazy_paramodulation::tableau

#endif  // LAZY_PARAMODULATION_PROVE_H_