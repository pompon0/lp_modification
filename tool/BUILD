load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

go_library(
    name = "go_default_library",
    srcs = ["tool.go"],
    data = [
        "//tptp_parser/src:tool",
        "//lazyparam_prover:tool",
    ],
    importpath = "github.com/pompon0/tptp_benchmark_go/tool",
    visibility = ["//visibility:public"],
    deps = [
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "//utils:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    srcs = ["tool_test.go"],
    embed = [":go_default_library"],
    deps = [
        "//eprover:go_default_library",
        "//problems:go_default_library",
        "//tptp_parser/proto:solutions_go_proto",
        "//tptp_parser/proto:tptp_go_proto",
        "@com_github_golang_protobuf//proto:go_default_library",
    ],
)

cc_binary(
  name = "limit",
  srcs = ["limit.cc"],
  deps = ["//lazyparam_prover/util:util"],
  visibility = ["//visibility:public"],
  #features = ["fully_static_link"],
  linkopts = [
    "-static-libstdc++",
    "-static-libgcc",
    "-l:libstdc++.a",
    "-l:libgcc.a",
  ],
)
