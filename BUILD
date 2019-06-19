load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")
load("@bazel_gazelle//:def.bzl", "gazelle")

# gazelle:prefix github.com/pompon0/tptp_benchmark_go
# gazelle:build_file_name BUILD
gazelle(name = "gazelle")

go_library(
    name = "go_default_library",
    srcs = ["main.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go",
    visibility = ["//visibility:private"],
    deps = [
        "//problems:go_default_library",
        "@org_golang_x_sync//errgroup:go_default_library",
    ],
)

go_binary(
    name = "tptp_benchmark_go",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)

go_proto_library(
    name = "tptp_go_proto",
    importpath = "github.com/pompon0/tptp_parser/proto/tptp_go_proto",
    proto = "@tptp_parser//proto:tptp_proto",
    visibility = ["//visibility:public"],
)
