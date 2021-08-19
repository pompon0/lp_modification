#define DEBUG_MODE
//#define VERBOSE
#include "gtest/gtest.h"
#include "utils/ctx.h"
#include "utils/log.h"
#include "problems/sample/sample.h"
#include "lazyparam_prover/controller/prover.h"
#include "lazyparam_prover/controller/features.h"
#include "ffprover/full_search.h"
#include "ffprover/tree.h"
#include "absl/time/time.h"
#include "tool/bin/wrapper.h"
#include <iostream>
#include <random>

using namespace ff;

struct FullSearchSuite : testing::TestWithParam<std::pair<const str,str>> {};
  
StreamLogger _(std::cerr);

TEST_P(FullSearchSuite,simple) {
  // DEBUG_MODE is too heavy to be used by default for this test
  // and gtest doesn't support parallelizable subtests.
  auto [n,tptpFOF] = GetParam();
  auto cnf = tool::bin::tptp_to_proto(tool::bin::tptp_cnf(tptpFOF));

  info("n = %",n);
  auto problem = controller::Problem::New(cnf);
  auto prover = controller::Prover::New(problem,1);
  auto tree = Tree::New();
  auto ctx = Ctx::with_timeout(Ctx::background(),absl::Seconds(10));
  auto res = FullSearch{}.run(ctx,tree->root(),*prover);
  ASSERT_EQ(Result::SOLVED,res.status);
  prover = controller::Prover::New(problem,1);
  collect_output(res.node,*prover,1.);
  ASSERT_TRUE(prover->done());
}

INSTANTIATE_TEST_SUITE_P(FullSearchSuiteInstance,FullSearchSuite,testing::ValuesIn(problems::sample::sample_problems()));
