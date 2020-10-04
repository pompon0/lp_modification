#define DEBUG_MODE
#include "gtest/gtest.h"
#include "absl/time/time.h"
#include "problems/sample/sample.h"
#include "tool/bin/wrapper.h"
#include "tool/bin/bin.pb.h"
#include "tptp.pb.h"

TEST(WRAPPER,simple) {
  StreamLogger _(std::cerr);
  for(auto [n,tptp] : problems::sample::sample_problems()) {
    tool::Request req;
    req.mutable_tptp_to_proto()->set_tptp(tptp);
    req.mutable_tptp_to_proto()->set_lang(tptp::Input::FOF);
    auto resp = tool::bin::wrapper(
      Ctx::with_timeout(Ctx::background(),absl::Seconds(2)),
      req
    );
  }
}
