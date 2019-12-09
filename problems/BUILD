load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = [
        "old.go",
        "problems.go",
        "proofs.go",
        "sample.go",
    ],
    data = [
        "@tptp_problems//file",
        "@mizar_problems//file",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/problems",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["problems_test.go"],
    shard_count = 2,
    embed = [":go_default_library"],
)
