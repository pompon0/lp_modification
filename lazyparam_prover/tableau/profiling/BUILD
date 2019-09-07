load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["case1.go"],
    data = [
        "//lazyparam_prover:main",
        "@tptp_sample//:problems",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau/profiling",
    visibility = ["//visibility:public"],
    deps = [
        "//lazyparam_prover/tableau:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//utils:go_default_library",
    ],
)

go_binary(
    name = "profiling",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
