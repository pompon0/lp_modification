load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["run.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/vampire/run",
    visibility = ["//visibility:private"],
    deps = [
        "//problems:go_default_library",
        "//vampire:go_default_library",
    ],
)

go_binary(
    name = "run",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
