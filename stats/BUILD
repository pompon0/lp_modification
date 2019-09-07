load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["stats.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/stats",
    visibility = ["//visibility:private"],
    deps = [
        "//problems:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
    ],
)

go_binary(
    name = "stats",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
