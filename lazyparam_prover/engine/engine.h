#ifndef ENGINE_ENGINE_H_
#define ENGINE_ENGINE_H_

namespace tableau::engine {

template<typename SEARCH> INL ProverOutput iterative_deepening(
  const Ctx &ctx,
  memory::Alloc &A,
  SearchState &s,
  SEARCH search
) { FRAME("iterative_deepening()");
  PROF_TIME("iterative_deepening");
  static_assert(memory::has_sig<SEARCH,bool(const Ctx&, memory::Alloc&, SearchState&, size_t limit)>());
  ProverOutput out;
  out.cost = 0; 
  for(;!ctx.done();) {
    auto As = A.save();
    auto ss = s.save();
    out.cost++; // avoid incrementing limit before context check
    DEBUG info("limit = %",out.cost);
    if(search(ctx,A,s,out.cost)) { 
      DEBUG_ONLY(
        str trace;
        size_t i = 0;
        for(auto l=s.trace; !l.empty(); l=l.tail()) {
          trace = util::fmt("[%] %\n",i++,l.head()) + trace;
        }
        info("\n%",trace);
      )
      out.cont_count = s.stats.steps;
      out.stats = s.stats;
      out.val = s.val.get_valuation();
      out.proof = s.get_proof(A);
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*out.proof));
      return out;
    }
    A.restore(As);
    s.restore(ss);
  }
  DEBUG info("FAILURE");
  out.cont_count = s.stats.steps;
  out.stats = s.stats;
  return out; 
}

}  // tableau::engine

#endif  // ENGINE_ENGINE_H_
