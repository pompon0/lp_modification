#define DEBUG if(1)
#include "gtest/gtest.h"

#include <filesystem>
#include "lazyparam_prover/util/log.h"
#include "lazyparam_prover/util/read_file.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/tableau.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/pred_format.h"

const str tptp_test_cnf_proto_path = "external/tptp_test_cnf_proto/";

using namespace util;

TEST(simple,all) {
  StreamLogger _(std::cerr);
  for(const auto &entry : std::filesystem::directory_iterator(tptp_test_cnf_proto_path)) {
    info("========================");
    info("%",entry.path());
    auto file_raw_bytes = util::read_file(entry.path());
    str file_raw(file_raw_bytes.begin(),file_raw_bytes.end());
    ParseCtx ctx;
    OrForm f(ctx.parse_notAndForm(file_raw));
    auto proof = prove_loop(f,20);
    ASSERT_TRUE(proof);
    ProtoCtx pctx(ctx);
    pctx.proto_notAndForm(NotAndForm(*proof));
  }
}

