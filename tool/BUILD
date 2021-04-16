load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = [
        "node.h",
        "parse2.h",
        "tool.go",
    ],
    data = [
        "//tptp_parser/src:tool",
        ":tool",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/tool",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["tool_test.go"],
    embed = [":go_default_library"],
    deps = [
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
    deps = [
        "//utils:utils",
        "//tptp_parser/proto:tptp_cc_proto",
        "@com_google_protobuf//:protobuf",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "parse2",
    hdrs = [
        "parse2.h",
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
    deps = [
        "//tptp_parser/proto:tptp_cc_proto",
        ":parse2",
        "//utils:utils",
        "@abseil//absl/flags:parse",
        "@abseil//absl/flags:flag",
    ],
    features = ["fully_static_link"],
)
