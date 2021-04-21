package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "engine",
    hdrs = [
        "engine.h",
        "balanced.h",
        "depth.h",
        "test.h",
    ],
    deps = [
        "//utils:utils",
        "//lazyparam_prover:prover",
        "//lazyparam_prover/syntax:syntax",
        "//lazyparam_prover/memory:memory",
        "//tptp_parser/proto:tptp_cc_proto",
        "//tptp_parser/proto:solutions_cc_proto",
    ],
)

cc_test(
    name = "test",
    srcs = [
      "test_test.cc",
      "balanced_test.cc",
      "depth_test.cc",
    ],
    deps = [
      "//lazyparam_prover/engine:engine",
      "@gtest//:gtest_main",
    ],
)
