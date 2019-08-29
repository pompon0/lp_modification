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
    deps = [":tptp_proto"],
)

proto_library(
    name = "proof_proto",
    srcs = ["proof.proto"],
    strip_import_prefix = "//tptp_parser/proto",
    visibility = ["//visibility:public"],
)

#########################################

cc_proto_library(
    name = "tptp_cc_proto",
    deps = [":tptp_proto"],
    visibility = ["//visibility:public"],
)

#########################################
load("@rules_haskell//haskell:protobuf.bzl", "haskell_proto_library")

haskell_proto_library(
    name = "tptp_haskell_proto",
    deps = [":tptp_proto"],
    visibility = ["//visibility:public"],
)

haskell_proto_library(
    name = "proof_haskell_proto",
    deps = [":proof_proto"],
    visibility = ["//visibility:public"],
)

haskell_proto_library(
    name = "solutions_haskell_proto",
    deps = [":solutions_proto"],
    visibility = ["//visibility:public"],
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
    deps = [":tptp_go_proto"],
)
