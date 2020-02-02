load("@io_bazel_rules_go//go:def.bzl", "go_library")

go_library(
    name = "go_default_library",
    srcs = [
        "bazel.go",
        "limit.go",
    ],
    data = [":limit"],
    importpath = "github.com/pompon0/tptp_benchmark_go/utils",
    visibility = ["//visibility:public"],
)

cc_binary(
    name = "limit",
    srcs = ["limit.cc"],
    deps = ["//lazyparam_prover/util:util"],
    visibility = ["//visibility:public"],
    features = ["fully_static_link"],
    linkopts = [
        "-static-libstdc++",
        "-static-libgcc",
        "-l:libstdc++.a",
        "-l:libgcc.a",
    ],
)
