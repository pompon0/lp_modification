load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

py_binary(
    name = "xgb-train",
    srcs = ["xgb-train.py"],
)

go_test(
    name = "go_default_test",
    srcs = ["xgbtrain_test.go"],
    data = [
        "//ffprover:mcts",
    ],
    embed = [":go_default_library"],
    deps = [
        "//eprover:go_default_library",
        "//ffprover:go_default_library",
        "//ffprover:mcts_go_proto",
        "//problems/sample:go_default_library",
        "//tool:go_default_library",
        "@org_golang_google_protobuf//types/known/durationpb:go_default_library",
    ],
)

go_library(
    name = "go_default_library",
    srcs = ["xgbtrain.go"],
    data = [":xgb-train"],
    importpath = "github.com/pompon0/tptp_benchmark_go/ffprover/xgbtrain",
    visibility = ["//visibility:public"],
    deps = [
        "//ffprover:mcts_go_proto",
        "//utils:go_default_library",
    ],
)
