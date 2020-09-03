#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/tool_wrapper.h"
#include "absl/time/time.h"

TEST(TOOL_WRAPPER,simple) {
  StreamLogger _(std::cerr);
  tptp::File f = tptp_to_proto(
    Ctx::with_timeout(Ctx::background(),absl::Seconds(2)),
    ""
  );
}
