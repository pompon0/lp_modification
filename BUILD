load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")
load("@bazel_gazelle//:def.bzl", "gazelle")

# gazelle:prefix github.com/pompon0/tptp_benchmark_go
# gazelle:build_file_name BUILD
gazelle(name = "gazelle")

go_proto_library(
    name = "tptp_go_proto",
    importpath = "github.com/pompon0/tptp_parser/proto/tptp_go_proto",
    proto = "@tptp_parser//proto:tptp_proto",
    visibility = ["//visibility:public"],
)

go_proto_library(
    name = "solutions_go_proto",
    importpath = "github.com/pompon0/tptp_parser/proto/solutions_go_proto",
    proto = "@tptp_parser//proto:solutions_proto",
    visibility = ["//visibility:public"],
    deps = [":tptp_go_proto"],
)
