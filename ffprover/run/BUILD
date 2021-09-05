load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["run.go"],
    importpath = "github.com/pompon0/tptp_benchmark_go/ffprover/run",
    visibility = ["//visibility:private"],
    deps = [
        "//eprover:go_default_library",
        "//ffprover:go_default_library",
        "//ffprover:mcts_go_proto",
        "//problems:go_default_library",
        "//tool:go_default_library",
        "@com_github_golang_protobuf//proto:go_default_library",
        "@org_golang_google_protobuf//types/known/durationpb:go_default_library",
    ],
)

go_binary(
    name = "run",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)

# NOTE: rules_pkg is builtin. If you want to import rules_pkg in WORKSPACE
#   use a different package name, like "rules_pkg2".
load("@rules_pkg//:pkg.bzl", "pkg_tar")

pkg_tar(
    name = "ffprover_tar",
    srcs = [":run"],
    include_runfiles = 1,
    remap_paths = {
        "external/": "",
        "": "__main__/",
    },
    strip_prefix = "/",
)
