package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "prover",
    hdrs = [
        "cont.h",
    ],
    deps = [
        "//utils:utils",
        "//tptp_parser/proto:tptp_cc_proto",
        "//tptp_parser/proto:solutions_cc_proto",
        "//lazyparam_prover:valuation",
        "//lazyparam_prover:parse",
        "//lazyparam_prover:prover",
        "//lazyparam_prover/connection_tableau/frames:frames",
        "//lazyparam_prover/syntax:syntax",
    ],
)

cc_test(
    name = "cont_test",
    srcs = ["cont_test.cc"],
    deps = [
        ":prover",
        "//lazyparam_prover/engine:engine",
        "@gtest//:gtest_main",
    ],
)
