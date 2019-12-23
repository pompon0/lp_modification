load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["benchmark.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/multibenchmark",
    visibility = ["//visibility:private"],
    deps = [
        "//eprover:go_default_library",
        "//lazyparam_prover/tableau:go_default_library",
        "//leancop:go_default_library",
        "//problems:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//vampire:go_default_library",
        "@com_github_golang_protobuf//ptypes:go_default_library_gen",
        "@org_golang_x_sync//errgroup:go_default_library",
    ],
)

go_binary(
    name = "multibenchmark",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
