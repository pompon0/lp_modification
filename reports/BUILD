load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["reports.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/reports",
    visibility = ["//visibility:private"],
    deps = [
        "//problems:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "@com_github_golang_protobuf//ptypes:go_default_library_gen",
    ],
)

go_binary(
    name = "reports",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
