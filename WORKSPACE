load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "tptp_parser",
  strip_prefix = "tptp_parser-master",
  urls = ["https://github.com/pompon0/tptp_parser/archive/master.zip"],
)

http_archive(
  name = "lazyparam_prover",
  strip_prefix = "lazyparam_prover-master",
  urls = ["https://github.com/pompon0/lazyparam_prover/archive/master.zip"],
)

load("@lazyparam_prover//:deps.bzl","rules_dependencies")
rules_dependencies()

http_archive(
    name = "io_bazel_rules_go",
    urls = ["https://github.com/bazelbuild/rules_go/releases/download/0.18.1/rules_go-0.18.1.tar.gz"],
    sha256 = "77dfd303492f2634de7a660445ee2d3de2960cbd52f97d8c0dffa9362d3ddef9",
)

http_archive(
    name = "bazel_gazelle",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/releases/download/0.17.0/bazel-gazelle-0.17.0.tar.gz"],
    sha256 = "3c681998538231a2d24d0c07ed5a7658cb72bfb5fd4bf9911157c0e9ac6a2687",
)

http_archive(
    name = "eprover",
    urls = [
      "http://wwwlehre.dhbw-stuttgart.de/~sschulz/WORK/E_DOWNLOAD/V_2.3/E.tgz",
      "https://storage.googleapis.com/tptp/eprover_2_3.tgz"
    ],
    sha256 = "5366d2de77e6856250e26a967642389e81a6f823caedccaf5022a09242aceb96",
    build_file = "eprover.BUILD",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies", "go_register_toolchains")

go_rules_dependencies()

go_register_toolchains()

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

gazelle_dependencies()

go_repository(
    name = "org_golang_x_sync",
    commit = "e225da77a7e68af35c70ccbf71af2b83e6acac3c",
    importpath = "golang.org/x/sync",
)
