load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = [
        "leancop.go",
        "prolog.go",
    ],
    data = [
        "@leancop//:leancop_bin",
        "@leancop_prolog//:leancop_src",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/leancop",
    visibility = ["//visibility:public"],
    deps = [
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//utils:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = [
        "leancop_test.go",
        "prolog_test.go",
    ],
    embed = [":go_default_library"],
    deps = ["//problems/sample:go_default_library"],
)
