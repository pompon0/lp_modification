package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "controller",
    hdrs = [
        "problem.h",
        "raw_prover.h",
        "prover.h",
        "features.h",
    ],
    deps = [
        "//lazyparam_prover:parse",
        "//lazyparam_prover:prover",
        "//tool/bin:wrapper",
        "//tool:node",
        "//utils:utils",
        "//lazyparam_prover/connection_tableau:prover",
    ],
)

cc_test(
    name = "raw_prover_test",
    srcs = ["raw_prover_test.cc"],
    deps = [
        ":controller",
        "//lazyparam_prover/engine:engine",
        "//utils:utils",
        "@gtest//:gtest_main",
    ],
)
