load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["run.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau/run",
    visibility = ["//visibility:private"],
    deps = [
        "//lazyparam_prover:prover_go_proto",
        "//lazyparam_prover/tableau:go_default_library",
        "//problems:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "//utils:go_default_library",
    ],
)

go_binary(
    name = "run",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
