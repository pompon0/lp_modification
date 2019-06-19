load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["cnf.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/cnf",
    visibility = ["//visibility:private"],
    deps = [
        "//problems:go_default_library",
        "//utils:go_default_library",
    ],
)

go_binary(
    name = "cnf",
    data = [
        "@eprover//:prover_bin",
    ],
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
