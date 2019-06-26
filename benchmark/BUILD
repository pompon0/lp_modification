load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["benchmark.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/benchmark",
    visibility = ["//visibility:private"],
    deps = [
        "//:tptp_go_proto",
        "//lazyparam_prover/tableau:go_default_library",
        "//problems:go_default_library",
        "//tool:go_default_library",
        "@org_golang_x_sync//errgroup:go_default_library",
    ],
)

go_binary(
    name = "benchmark",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
