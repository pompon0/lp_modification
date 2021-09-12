//#define DEBUG_MODE
//#define VERBOSE
#include "gtest/gtest.h"
#include "utils/ctx.h"
#include "utils/log.h"
#include "problems/sample/sample.h"
#include "lazyparam_prover/controller/prover.h"
#include "lazyparam_prover/controller/features.h"
#include "ffprover/search.h"
#include "ffprover/tree.h"
#include "absl/time/time.h"
#include "tool/bin/wrapper.h"
#include <iostream>
#include <random>

using namespace ff;

Search::Config cfg {
  .one_expansion_per_playout = false,
  .playouts_per_bigstep = 20,
  .playout_depth = 40,
  .base_reward = [](features::StateVec f){ return 0.3; },
  .base_priority = [](features::ActionVec f){ return 1.; },
  .child_priority = [](Tree::Ptr t, size_t i) {
    return -double(t.child(i).visits());
  },
  .bigstep_selector = [](Tree::Ptr t) -> size_t {
    if(t.child_count()==1) return 0;
    ssize_t max_i = -1;
    double max_p = -1;
    for(size_t i=0; i<t.child_count(); i++) {
      if(auto w = t.child(i).won_depth(); w && w.get()==t.won_depth().get()-1) return i;
      auto visits = t.child(i).visits();
      if(visits==0) continue;
      // choose by visits (+rewards tie breaker)
      double p = double(visits) + t.child(i).rewards()/double(visits);
      if(p>max_p){ max_p = p; max_i = i; }
    }
    if(max_i==-1) error("no candidate for bigstep");
    return size_t(max_i);
  }
};

struct SearchSuite : testing::TestWithParam<std::pair<const str,str>> {};

TEST_P(SearchSuite,simple) {
  StreamLogger _(std::cerr);
  // DEBUG_MODE is too heavy to be used by default for this test
  // and gtest doesn't support parallelizable subtests.
  auto [n,tptpFOF] = GetParam();
  info("n = %",n);
  auto cnf = tool::bin::tptp_to_proto(tool::bin::tptp_cnf(tptpFOF));
  auto problem = controller::Problem::New(cnf);
  auto prover = controller::Prover::New(problem,32);
  auto tree = Tree::New();
  Search search(cfg);
  auto ctx = Ctx::with_timeout(Ctx::background(),absl::Seconds(10));
  auto res = search.run(ctx,tree->root(),*prover);
  ASSERT_EQ(Result::SOLVED,res.status);
  prover = controller::Prover::New(problem,1);
  mcts::Path path;
  collect_output(&path,res.node,*prover,1.);
  ASSERT_TRUE(prover->done());
}

INSTANTIATE_TEST_SUITE_P(SearchSuiteInstance,SearchSuite,testing::ValuesIn(problems::sample::sample_problems()));
