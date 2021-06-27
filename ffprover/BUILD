load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

cc_library(
    name = "mcts_lib",
    hdrs = [
        "xgboost.h",
        "tree.h",
        "search.h",
    ],
    deps = [
        "//lazyparam_prover/controller:controller",
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

# NOTE: rules_pkg is builtin. If you want to import rules_pkg in WORKSPACE
#   use a different package name, like "rules_pkg2".
load("@rules_pkg//:pkg.bzl", "pkg_tar")

pkg_tar(
    name = "ffprover_tar",
    srcs = [":ffprover"],
    strip_prefix = "/",
    include_runfiles = 1,
    remap_paths = {
        "external/": "",
        "": "__main__/",
    }
)
