package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "controller",
    hdrs = [
        "features.h",
        "problem.h",
        "prover.h",
        "raw_prover.h",
    ],
    deps = [
        "//lazyparam_prover:parse",
        "//lazyparam_prover:prover",
        "//lazyparam_prover/connection_tableau:prover",
        "//tool:node",
        "//tool/bin:wrapper",
        "//utils",
    ],
)

cc_test(
    name = "raw_prover_test",
    srcs = ["raw_prover_test.cc"],
    deps = [
        ":controller",
        "//lazyparam_prover/engine",
        "//utils",
        "@gtest//:gtest_main",
    ],
)
