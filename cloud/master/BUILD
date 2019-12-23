load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["master.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/cloud/master",
    visibility = ["//visibility:private"],
    deps = [
        "//cloud/worker:worker_go_proto",
        "//problems:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "@com_github_golang_protobuf//ptypes:go_default_library_gen",
        "@org_golang_google_grpc//:go_default_library",
        "@org_golang_google_grpc//codes:go_default_library",
        "@org_golang_google_grpc//credentials:go_default_library",
        "@org_golang_google_grpc//credentials/oauth:go_default_library",
        "@org_golang_google_grpc//status:go_default_library",
        "@org_golang_x_sync//errgroup:go_default_library",
        "@org_golang_x_sync//semaphore:go_default_library",
    ],
)

go_binary(
    name = "master",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)
