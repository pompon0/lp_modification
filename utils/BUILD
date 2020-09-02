load("@io_bazel_rules_go//go:def.bzl", "go_library")

go_library(
    name = "go_default_library",
    srcs = [
        "bazel.go",
        "enum.go",
        "limit.go",
    ],
    data = [":limit"],
    importpath = "github.com/pompon0/tptp_benchmark_go/utils",
    visibility = ["//visibility:public"],
    deps = ["@org_golang_google_protobuf//reflect/protoreflect:go_default_library"],
)

cc_binary(
    name = "limit",
    srcs = ["limit.cc"],
    deps = ["//lazyparam_prover/util:util"],
    features = ["fully_static_link"],
    visibility = ["//visibility:public"],
)
