load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["vampire.go"],
    data = ["@vampire//:vampire"],
    importpath = "github.com/pompon0/tptp_benchmark_go/vampire",
    visibility = ["//visibility:public"],
    deps = ["//utils:go_default_library"],
)

go_test(
    name = "go_default_test",
    srcs = ["vampire_test.go"],
    embed = [":go_default_library"],
    deps = ["//problems:go_default_library"],
)
