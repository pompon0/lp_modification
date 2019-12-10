load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")
load("@io_bazel_rules_docker//go:image.bzl", "go_image")
load("@io_bazel_rules_docker//container:container.bzl", "container_push")

proto_library(
    name = "worker_proto",
    srcs = ["worker.proto"],
    visibility = ["//visibility:public"],
)

go_proto_library(
    name = "worker_go_proto",
    compilers = ["@io_bazel_rules_go//proto:go_grpc"],
    importpath = "github.com/pompon0/tptp_benchmark_go/worker/worker_go_proto",
    proto = ":worker_proto",
    visibility = ["//visibility:public"],
)

go_library(
    name = "go_default_library",
    srcs = ["worker.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/cloud/worker",
    visibility = ["//visibility:private"],
    deps = [
        ":worker_go_proto",
        "@org_golang_google_grpc//:go_default_library",
    ],
)

go_binary(
    name = "worker",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)

go_image(
    name = "worker_img",
    binary = ":worker",
    args = [],
)

# run "gcloud auth configure-docker" to be able to push
container_push(
    name = "worker_push",
    image = ":worker_img",
    format = "Docker",
    registry = "gcr.io",
    repository = "mapstore-201213/worker",
)
