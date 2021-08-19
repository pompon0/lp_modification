load("@rules_proto//proto:defs.bzl", "proto_library")

proto_library(
    name = "tptp_proto",
    srcs = ["tptp.proto"],
    strip_import_prefix = "//tptp_parser/proto",
    visibility = ["//visibility:public"],
)

proto_library(
    name = "solutions_proto",
    srcs = ["solutions.proto"],
    strip_import_prefix = "//tptp_parser/proto",
    visibility = ["//visibility:public"],
    deps = [
        ":tptp_proto",
        "@com_google_protobuf//:duration_proto",
        "@com_google_protobuf//:timestamp_proto",
    ],
)

#########################################

cc_proto_library(
    name = "tptp_cc_proto",
    visibility = ["//visibility:public"],
    deps = [":tptp_proto"],
)

cc_proto_library(
    name = "solutions_cc_proto",
    visibility = ["//visibility:public"],
    deps = [":solutions_proto"],
)

#########################################
load("@rules_haskell//haskell:protobuf.bzl", "haskell_proto_library")

haskell_proto_library(
    name = "tptp_haskell_proto",
    visibility = ["//visibility:public"],
    deps = [":tptp_proto"],
)

haskell_proto_library(
    name = "solutions_haskell_proto",
    visibility = ["//visibility:public"],
    deps = [":solutions_proto"],
)

#########################################
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")

go_proto_library(
    name = "tptp_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto",
    proto = ":tptp_proto",
    visibility = ["//visibility:public"],
)

go_proto_library(
    name = "solutions_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto",
    proto = ":solutions_proto",
    visibility = ["//visibility:public"],
    deps = [
        ":tptp_go_proto",
        "@com_github_golang_protobuf//ptypes/duration",
        "@com_github_golang_protobuf//ptypes/timestamp",
    ],
)
