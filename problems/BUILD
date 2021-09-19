load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = [
        "problems.go",
        "proofs.go",
        "vampire_no_eq.go",
    ],
    data = [
        "@mizar_problems//file",
        "@tptp_problems//file",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/problems",
    visibility = ["//visibility:public"],
    deps = [
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    size = "large",
    srcs = ["problems_test.go"],
    embed = [":go_default_library"],
    tags = ["manual"],
    deps = [
        "//eprover:go_default_library",
        "//tool:go_default_library",
    ],
)
