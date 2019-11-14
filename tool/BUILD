load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["tool.go"],
    data = ["//tptp_parser/src:tool"],
    importpath = "github.com/pompon0/tptp_benchmark_go/tool",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["tool_test.go"],
    embed = [":go_default_library"],
    deps = [
        "//problems:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)
