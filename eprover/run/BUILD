load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["run.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/eprover/run",
    visibility = ["//visibility:private"],
    deps = [
        "//eprover:go_default_library",
        "//problems:go_default_library",
    ],
)

go_binary(
    name = "run",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
