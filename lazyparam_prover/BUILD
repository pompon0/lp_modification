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
  name = "pred",
  hdrs = [
    "alloc.h",
    "pred.h",
  ],
  deps = [
    ":log",
    ":types",
  ],
)

cc_library(
  name = "parse2",
  hdrs = ["parse2.h"],
  deps = [
    ":types",
    ":pred",
    "@vampire//:lib",
    "@com_google_protobuf//:protobuf"
  ],
)

cc_binary(
  name = "tool",
  srcs = ["tool.cc"],
  deps = [
    ":parse2",
    "@abseil//absl/flags:parse",
    "@abseil//absl/flags:flag",
  ],
)

cc_library(
    name = "prover",
    hdrs = [
        "ctx.h",
        "alt.h",
        "mgu.h",
        "ground.h",
        "parse.h",
        "pred_format.h",
        "kbo.h",
        "tableau.h",
        "eq_axioms.h",
    ],
    deps = [
        "//tptp_parser/proto:tptp_cc_proto",
        "//tptp_parser/proto:solutions_cc_proto",
        ":pred",
        ":log",
        ":types",
        "//lazyparam_prover/util:util",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_test(
    name = "alloc_test",
    srcs = ["alloc_test.cc"],
    deps = [
        ":prover",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
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
    name = "kbo_test",
    srcs = ["kbo_test.cc"],
    copts = ["--std=c++17"],
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
    features = ["fully_static_link"],
    linkopts = [
        "-static-libstdc++",
        "-static-libgcc",
        "-l:libstdc++.a",
    ],
    visibility = ["//visibility:public"],
)
