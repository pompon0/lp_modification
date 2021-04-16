package(
    default_visibility = ["//visibility:public"],
)

load("@io_bazel_rules_go//go:def.bzl", "go_library")

go_library(
    name = "go_default_library",
    srcs = [
        "bazel.go",
        "bazel.h",
        "ctx.h",
        "enum.go",
        "enum_flag.h",
        "limit.go",
        "log.h",
        "number_theory.h",
        "read_file.h",
        "short.h",
        "string.h",
        "sys.h",
        "types.h",
    ],
    data = [":limit"],
    importpath = "github.com/pompon0/tptp_benchmark_go/utils",
    visibility = ["//visibility:public"],
    deps = ["@org_golang_google_protobuf//reflect/protoreflect:go_default_library"],
)

cc_binary(
    name = "limit",
    srcs = ["limit.cc"],
    deps = [":utils"],
    features = ["fully_static_link"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "utils",
    hdrs = [
        "bazel.h",
        "ctx.h",
        "enum_flag.h",
        "log.h",
        "number_theory.h",
        "read_file.h",
        "short.h",
        "string.h",
        "sys.h",
        "types.h",
    ],
    srcs = ["log.cc"],
    deps = [
        "@abseil//absl/strings:strings",
        "@abseil//absl/time:time",
        "@abseil//absl/types:optional",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_test(
    name = "number_theory_test",
    srcs = ["number_theory_test.cc"],
    deps = [
        ":utils",
        "@gtest//:gtest_main",
    ],
)
