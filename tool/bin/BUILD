load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")

go_library(
    name = "go_default_library",
    srcs = [
        "bin.go",
        "wrapper.h",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/tool/bin",
    visibility = ["//visibility:private"],
    deps = [
        ":bin_go_proto",
        "//eprover:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:tptp_go_proto",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

proto_library(
    name = "bin_proto",
    srcs = ["bin.proto"],
    deps = ["//tptp_parser/proto:tptp_proto"],
)

go_proto_library(
    name = "bin_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/tool/bin/bin_go_proto",
    proto = ":bin_proto",
    deps = ["//tptp_parser/proto:tptp_go_proto"],
)

cc_proto_library(
    name = "bin_cc_proto",
    deps = ["bin_proto"],
)

cc_library(
    name = "wrapper",
    data = [":bin"],
    hdrs = ["wrapper.h"],
    deps = [
        ":bin_cc_proto",
        "//utils:utils",
    ],
    visibility = ["//visibility:public"],
)

cc_test(
    name = "wrapper_test",
    srcs = ["wrapper_test.cc"],
    deps = [
        ":wrapper",
        ":bin_cc_proto",
        "//utils:utils",
        "//problems/sample:sample_cc",
        "@gtest//:gtest_main",
    ],
)

go_binary(
    name = "bin",
    embed = [":go_default_library"],
    visibility = ["//visibility:private"],  # keep
)
