load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

cc_library(
    name = "mcts_lib",
    hdrs = [
        "xgboost.h",
        "tree.h",
        "search.h",
    ],
    deps = [
        "//lazyparam_prover:controller",
        "//utils:utils",
        "@abseil//absl/flags:parse",
        "@abseil//absl/flags:flag",
        "@abseil//absl/time:time",
        "@xgboost//:xgboost",
    ],
)

cc_test(
    name = "tree_test",
    srcs = ["tree_test.cc"],
    deps = [
        ":mcts_lib",
        "//utils:utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "search_test",
    srcs = ["search_test.cc"],
    deps = [
        ":mcts_lib",
        "//utils:utils",
        "//problems/sample:sample_cc",
        "@gtest//:gtest_main",
    ],
)

cc_import(
    name = "openmp",
    static_library = "libomp.a",
)

cc_binary(
    name = "mcts",
    deps = [
        ":mcts_lib",
    ],
    srcs = ["mcts.cc"],
    features = ["fully_static_link"],
)

go_library(
    name = "go_default_library",
    srcs = [
        "mcts.go",
        "search.h",
        "tree.h",
        "xgboost.h",
    ],
    data = [":mcts"],
    importpath = "github.com/pompon0/tptp_benchmark_go/ffprover",
    visibility = ["//visibility:private"],
    deps = [
        "//problems:go_default_library",
        "//tool:go_default_library",
        "//utils:go_default_library",
        "@com_github_google_uuid//:go_default_library",
    ],
)

go_binary(
    name = "ffprover",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
