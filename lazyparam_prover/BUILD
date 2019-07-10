cc_proto_library(
    name = "tptp_cc_proto",
    deps = ["@tptp_parser//proto:tptp_proto"],
)

cc_library(
    name = "types",
    hdrs = ["types.h"],
)

cc_library(
    name = "log",
    hdrs = ["log.h"],
    srcs = ["log.cc"],
    deps = [
        ":types",
        "//lazyparam_prover/util:util",
    ],
)

cc_library(
    name = "prover",
    hdrs = [
        "mgu.h",
        "ground.h",
        "alloc.h",
        "pred.h",
        "pred_format.h",
        "kbo.h",
        "parse.h",
        "tableau.h",
        "eq_axioms.h",
    ],
    deps = [
        ":tptp_cc_proto",
        ":log",
        ":types",
        "//lazyparam_prover/util:util",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_test(
    name = "mgu_test",
    srcs = ["mgu_test.cc"],
    deps = [
        ":prover",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "pred_test",
    srcs = ["pred_test.cc"],
    deps = [
        ":prover",
        "@gtest//:gtest_main",
    ],
)

"""cc_test(
  name = "simple_test",
  srcs = ["simple_test.cc"],
  data = ["@tptp_test_cnf_proto//:problems"],
  copts = ["--std=c++17"],
  linkopts = ["-lstdc++fs"],
  deps = [
    ":prover",
    "//lazyparam_prover/util:util",
    "@gtest//:gtest_main",
  ],
)"""

cc_test(
    name = "eq_axioms_test",
    srcs = ["eq_axioms_test.cc"],
    deps = [
        ":prover",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)

cc_binary(
    name = "main",
    srcs = ["main.cc"],
    deps = [
        ":prover",
        "@abseil//absl/flags:flag",
        "@abseil//absl/flags:parse",
    ],
    visibility = ["//visibility:public"],
)
