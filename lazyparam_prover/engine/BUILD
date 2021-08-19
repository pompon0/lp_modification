package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "engine",
    hdrs = [
        "balanced.h",
        "depth.h",
        "engine.h",
        "test.h",
    ],
    deps = [
        "//lazyparam_prover:prover",
        "//lazyparam_prover/memory",
        "//lazyparam_prover/syntax",
        "//tptp_parser/proto:solutions_cc_proto",
        "//tptp_parser/proto:tptp_cc_proto",
        "//utils",
    ],
)

cc_test(
    name = "test",
    srcs = [
        "balanced_test.cc",
        "depth_test.cc",
        "test_test.cc",
    ],
    deps = [
        "//lazyparam_prover/engine",
        "@gtest//:gtest_main",
    ],
)
