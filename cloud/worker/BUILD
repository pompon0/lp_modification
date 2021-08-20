load("@rules_proto//proto:defs.bzl", "proto_library")
load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")
load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")
load("@io_bazel_rules_docker//go:image.bzl", "go_image")
load("@io_bazel_rules_docker//container:container.bzl", "container_push")

proto_library(
    name = "worker_proto",
    srcs = ["worker.proto"],
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_proto",
        "@com_google_protobuf//:duration_proto",
    ],
)

go_proto_library(
    name = "worker_go_proto",
    compilers = ["@io_bazel_rules_go//proto:go_grpc"],
    importpath = "github.com/pompon0/tptp_benchmark_go/cloud/worker/worker_go_proto",
    proto = ":worker_proto",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
    ],
)

go_library(
    name = "go_default_library",
    srcs = ["worker.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/cloud/worker",
    visibility = ["//visibility:private"],
    deps = [
        ":worker_go_proto",
        "//eprover:go_default_library",
        "//ffprover:go_default_library",
        "//lazyparam_prover/tableau:go_default_library",
        "//leancop:go_default_library",
        "//tool:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//vampire:go_default_library",
        "@org_golang_google_grpc//:go_default_library",
        "@org_golang_google_grpc//codes:go_default_library",
        "@org_golang_google_grpc//status:go_default_library",
        "@org_golang_google_protobuf//types/known/durationpb:go_default_library",
        "@org_golang_x_sync//semaphore:go_default_library",
    ],
)

go_binary(
    name = "worker",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)

go_image(
    name = "worker_img",
    args = [],
    base = "@swipl//image",
    binary = ":worker",
    visibility = ["//visibility:public"],
)

# run "gcloud auth configure-docker" to be able to push
container_push(
    name = "worker_push",
    format = "Docker",
    image = ":worker_img",
    registry = "gcr.io",
    repository = "mapstore-201213/worker",
)
