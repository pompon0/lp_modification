load("@io_bazel_rules_go//go:def.bzl", "go_library")

go_library(
    name = "go_default_library",
    srcs = ["push.go"],
    data = ["//cloud/worker:worker_img.tar"],
    importpath = "github.com/pompon0/tptp_benchmark_go/cloud/worker/push",
    visibility = ["//visibility:public"],
    deps = [
        "//utils:go_default_library",
        "@com_github_google_go_containerregistry//pkg/authn:go_default_library",
        "@com_github_google_go_containerregistry//pkg/name:go_default_library",
        "@com_github_google_go_containerregistry//pkg/v1/remote:go_default_library",
        "@com_github_google_go_containerregistry//pkg/v1/remote/transport:go_default_library",
        "@com_github_google_go_containerregistry//pkg/v1/tarball:go_default_library",
        "@org_golang_google_api//googleapi:go_default_library",
        "@org_golang_google_api//option:go_default_library",
        "@org_golang_google_api//run/v1:go_default_library",
    ],
)
