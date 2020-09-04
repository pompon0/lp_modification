#define DEBUG_MODE
#include "gtest/gtest.h"
#include "absl/time/time.h"
#include "problems/sample/sample.h"
#include "lazyparam_prover/tool_wrapper.h"

TEST(TOOL_WRAPPER,simple) {
  StreamLogger _(std::cerr);
  for(auto [n,tptp] : problems::sample::sample_problems()) {
    tptp::File f = tool_wrapper::tptp_to_proto(
      Ctx::with_timeout(Ctx::background(),absl::Seconds(2)),
      tptp
    );
  }
}
