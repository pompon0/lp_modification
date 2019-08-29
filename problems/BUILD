load("@io_bazel_rules_go//go:def.bzl", "go_library")

go_library(
    name = "go_default_library",
    srcs = [
        "problems.go",
        "proofs.go",
        "sample.go",
    ],
    data = ["@tptp_sample//:problems"],
    importpath = "github.com/pompon0/tptp_benchmark_go/problems",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)
