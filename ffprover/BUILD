load("@rules_proto//proto:defs.bzl", "proto_library")
load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")

proto_library(
    name = "mcts_proto",
    srcs = ["mcts.proto"],
    deps = [
        "//tptp_parser/proto:solutions_proto",
        "//tptp_parser/proto:tptp_proto",
        "@com_google_protobuf//:duration_proto",
    ],
)

cc_proto_library(
    name = "mcts_cc_proto",
    deps = [":mcts_proto"],
)

go_proto_library(
    name = "mcts_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto",
    proto = ":mcts_proto",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "@com_github_golang_protobuf//ptypes/duration",
    ],
)

cc_library(
    name = "mcts_lib",
    hdrs = [
        "full_search.h",
        "search.h",
        "tree.h",
        "xgboost.h",
    ],
    deps = [
        ":mcts_cc_proto",
        "//lazyparam_prover/controller",
        "//utils",
        "@abseil//absl/flags:flag",
        "@abseil//absl/flags:parse",
        "@abseil//absl/time",
        "@xgboost",
    ],
)

cc_test(
    name = "tree_test",
    srcs = ["tree_test.cc"],
    deps = [
        ":mcts_lib",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    # separate target, since it has
    # DEBUG_MODE disabled.
    name = "search_test",
    srcs = [
        "search_test.cc",
    ],
    deps = [
        ":mcts_lib",
        "//problems/sample:sample_cc",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "full_search_test",
    srcs = [
        "full_search_test.cc",
    ],
    # Force optimization, as it is too slow without it
    # and I don't know how to parallelize TEST_P yet.
    copts = ["-O3"],
    deps = [
        ":mcts_lib",
        "//problems/sample:sample_cc",
        "//tool/bin:wrapper",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_binary(
    name = "mcts",
    srcs = ["mcts.cc"],
    features = ["fully_static_link"],
    visibility = ["//ffprover:__subpackages__"],
    deps = [
        ":mcts_cc_proto",
        ":mcts_lib",
    ],
)

go_library(
    name = "go_default_library",
    srcs = [
        "full_search.h",
        "mcts.go",
        "search.h",
        "tree.h",
        "xgboost.h",
    ],
    data = [":mcts"],
    importpath = "github.com/pompon0/tptp_benchmark_go/ffprover",
    visibility = ["//visibility:public"],
    deps = [
        ":mcts_go_proto",
        "//eprover:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
        "@org_golang_google_protobuf//types/known/durationpb:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["mcts_test.go"],
    embed = [":go_default_library"],
    deps = ["//problems/sample:go_default_library"],
)
