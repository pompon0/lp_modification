load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = [
        "conv.h",
        "node.h",
        "tool.go",
    ],
    data = [
        ":tool",
        "//tptp_parser/src:tool",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/tool",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
        "@org_golang_x_sync//semaphore:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["tool_test.go"],
    deps = [
        ":go_default_library",
        "//eprover:go_default_library",
        "//problems/sample:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "@com_github_google_go_cmp//cmp:go_default_library",
    ],
)

cc_library(
    name = "node",
    hdrs = ["node.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:tptp_cc_proto",
        "//utils",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_library(
    name = "conv",
    hdrs = [
        "conv.h",
    ],
    deps = [
        ":node",
        "//tptp_parser/proto:tptp_cc_proto",
        # vampire agressively takes over signal handling of the whole binary
        # in particular it severly reduces the global memory limits.
        # DO NOT LINK into actual prover, until you refector it.
        "@vampire//:lib",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_binary(
    name = "tool",
    srcs = ["tool.cc"],
    features = ["fully_static_link"],
    deps = [
        ":conv",
        ":node",
        "//tptp_parser/proto:tptp_cc_proto",
        "//utils",
        "@abseil//absl/flags:flag",
        "@abseil//absl/flags:parse",
    ],
)
