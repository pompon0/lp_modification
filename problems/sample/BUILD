load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")

proto_library(
    name = "sample_proto",
    srcs = ["sample.proto"],
)

filegroup(
    name = "sample_textpb",
    srcs = ["sample.textpb"],
)

go_proto_library(
    name = "sample_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/problems/sample/sample_go_proto",
    proto = ":sample_proto",
)

cc_proto_library(
    name = "sample_cc_proto",
    deps = [":sample_proto"],
)

go_library(
    name = "go_default_library",
    srcs = ["sample.go"],
    data = [":sample_textpb"],
    importpath = "github.com/pompon0/tptp_benchmark_go/problems/sample",
    visibility = ["//visibility:public"],
    deps = [
        ":sample_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["sample_test.go"],
    embed = [":go_default_library"],
    deps = ["//tool:go_default_library"],
)

cc_library(
    name = "sample_cc",
    hdrs = ["sample.h"],
    data = [":sample_textpb"],
    deps = [
      "//utils:utils",
      ":sample_cc_proto",
      "@com_google_protobuf//:protobuf",
    ],
    visibility = ["//visibility:public"],
)

cc_test(
    name = "sample_cc_test",
    srcs = ["sample_test.cc"],
    deps = [
      ":sample_cc",
      "//utils:utils",
      "@gtest//:gtest_main",
    ],
)

