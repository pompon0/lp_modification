load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["tableau.go"],
    data = [
        "//lazyparam_prover:main",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau",
    visibility = ["//visibility:public"],
    deps = [
        "//:tptp_go_proto",
        "//tool:go_default_library",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["tableau_test.go"],
    embed = [":go_default_library"],
    deps = [
        "//problems:go_default_library",
        "//tool:go_default_library",
    ],
)
