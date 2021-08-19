package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "prover",
    hdrs = [
        "cont.h",
        "split.h",
    ],
    deps = [
        "//lazyparam_prover:parse",
        "//lazyparam_prover:prover",
        "//lazyparam_prover:valuation",
        "//lazyparam_prover/lazy_paramodulation/frames",
        "//lazyparam_prover/syntax",
        "//tptp_parser/proto:solutions_cc_proto",
        "//tptp_parser/proto:tptp_cc_proto",
        "//utils",
    ],
)

cc_test(
    name = "cont_test",
    srcs = ["cont_test.cc"],
    deps = [
        ":prover",
        "//lazyparam_prover/engine",
        "@gtest//:gtest_main",
    ],
)
