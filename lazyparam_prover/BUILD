load("@rules_proto//proto:defs.bzl", "proto_library")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")

package(
    default_visibility = ["//visibility:public"],
)

proto_library(
    name = "prover_proto",
    srcs = ["prover.proto"],
)

cc_proto_library(
    name = "prover_cc_proto",
    deps = [":prover_proto"],
)

go_proto_library(
    name = "prover_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/prover_go_proto",
    proto = ":prover_proto",
)

cc_library(
    name = "derived",
    hdrs = [
        "constraint.h",
        "derived.h",
    ],
    deps = [
        "//lazyparam_prover/memory",
        "//lazyparam_prover/syntax",
        "//utils",
    ],
)

cc_test(
    name = "derived_test",
    srcs = [
        "constraint_test.cc",
        "derived_test.cc",
    ],
    deps = [
        ":derived",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_library(
    name = "parse",
    hdrs = [
        "parse.h",
    ],
    deps = [
        "//lazyparam_prover/syntax",
        "//tool:node",
        "//tptp_parser/proto:tptp_cc_proto",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_library(
    name = "valuation",
    hdrs = [
        "constrained_valuation.h",
        "constraint.h",
        "fun_ord.h",
        "kbo.h",
        "lpo.h",
        "rewrite_test_utils.h",
        "valuation.h",
    ],
    deps = [
        ":derived",
        "//lazyparam_prover/memory",
        "//lazyparam_prover/syntax",
        "//tptp_parser/proto:solutions_cc_proto",
    ],
)

cc_library(
    name = "prover",
    hdrs = [
        "eq_axioms.h",
        "ground.h",
        "index.h",
        "lpmod.h",
        "prover_output.h",
        "search_state.h",
    ],
    deps = [
        ":parse",
        ":valuation",
        "//lazyparam_prover/syntax",
        "//tptp_parser/proto:solutions_cc_proto",
        "//tptp_parser/proto:tptp_cc_proto",
        "//utils",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_test(
    name = "valuation_test",
    srcs = ["valuation_test.cc"],
    deps = [
        ":valuation",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "kbo_test",
    srcs = ["kbo_test.cc"],
    deps = [
        ":valuation",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "lpo_test",
    srcs = ["lpo_test.cc"],
    deps = [
        ":valuation",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "eq_axioms_test",
    srcs = ["eq_axioms_test.cc"],
    deps = [
        ":prover",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_binary(
    name = "main",
    srcs = ["main.cc"],
    features = ["fully_static_link"],
    visibility = ["//visibility:public"],
    deps = [
        ":prover",
        ":prover_cc_proto",
        "//lazyparam_prover/connection_tableau:prover",
        "//lazyparam_prover/engine",
        "//lazyparam_prover/lazy_paramodulation:prover",
        "//utils",
        "@abseil//absl/flags:flag",
        "@abseil//absl/flags:parse",
    ],
)
