load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["tptp_filter.go"],
    data = ["@tptp4X//file"],
    importpath = "github.com/pompon0/tptp_benchmark_go/problems/tptp_filter",
    visibility = ["//visibility:public"],
    deps = [
        "//eprover:go_default_library",
        "//tool:go_default_library",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_binary(
    name = "tptp_filter",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
