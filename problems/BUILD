load("@io_bazel_rules_go//go:def.bzl", "go_library")

go_library(
    name = "go_default_library",
    srcs = [
        "problems.go",
        "sample.go",
    ],
    data = ["@tptp_sample//:problems"],
    importpath = "github.com/pompon0/tptp_benchmark_go/problems",
    visibility = ["//visibility:public"],
    deps = ["//utils:go_default_library"],
)
