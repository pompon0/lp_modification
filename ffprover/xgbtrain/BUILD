load("@io_bazel_rules_go//go:def.bzl", "go_test")

py_binary(
    name = "xgb-train",
    srcs = ["xgb-train.py"],
)

go_test(
    name = "go_default_test",
    srcs = ["xgbtrain_test.go"],
    data = [
        ":xgb-train",
        "//ffprover:mcts",
    ],
    deps = [
        "//problems/sample:go_default_library",
        "//tool:go_default_library",
        "//utils:go_default_library",
    ],
)
