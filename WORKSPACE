load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "eprover",
  urls = [
    "http://wwwlehre.dhbw-stuttgart.de/~sschulz/WORK/E_DOWNLOAD/V_2.3/E.tgz",
    "https://storage.googleapis.com/tptp/eprover_2_3.tgz",
  ],
  sha256 = "5366d2de77e6856250e26a967642389e81a6f823caedccaf5022a09242aceb96",
  build_file = "//:third_party/eprover.BUILD",
)

http_archive(
    name = "leancop",
    urls = ["https://storage.googleapis.com/tptp/leancop_bin.tgz"],
    sha256 = "c8c154c7f694ffd5eee7453bb50bd07f5eac0b4c564a5b9a0cb548e802ed7bbf",
    build_file = "//:third_party/leancop.BUILD",
)

http_archive(
    name = "leancop_prolog",
    urls = [
      "http://www.leancop.de/programs/leancop21.tar.gz",
      "https://storage.googleapis.com/tptp/leancop21.tar.gz",
    ],
    sha256 = "ce432c5f9368c093f08df3120218463d8bdb8412e71ec980ab9c852c13cef300",
    build_file = "//:third_party/leancop_prolog.BUILD",
)

http_archive(
    name = "tptp_sample",
    urls = ["https://storage.googleapis.com/tptp/tptp_sample.tgz"],
    sha256 = "81909a2753f658edcd6f0a3b10b82178c845a58665c1f3d08a583e3e644c298c",
    build_file = "//:third_party/tptp_sample.BUILD",
)

http_archive(
  name = "com_google_protobuf",
  strip_prefix = "protobuf-3.9.0",
  urls = ["https://github.com/google/protobuf/archive/v3.9.0.zip"],
  sha256 = "8eb5ca331ab8ca0da2baea7fc0607d86c46c80845deca57109a5d637ccb93bb4",
)
load("@com_google_protobuf//:protobuf_deps.bzl","protobuf_deps")
protobuf_deps()

http_archive(
  name = "gtest",
  strip_prefix = "googletest-release-1.8.1",
  urls = ["https://github.com/google/googletest/archive/release-1.8.1.zip"],
  sha256 = "927827c183d01734cc5cfef85e0ff3f5a92ffe6188e0d18e909c5efebf28a0c7",
)

http_archive(
  name = "abseil",
  strip_prefix = "abseil-cpp-d659fe54b35ab9b8e35c72e50a4b8814167d5a84",
  urls = ["https://github.com/abseil/abseil-cpp/archive/d659fe54b35ab9b8e35c72e50a4b8814167d5a84.zip"],
  sha256 = "743f879030960c80a0445310567ca99d8eae87468dd0e78fd0c8bfd46f429e37",
)

http_archive(
  name = "rules_cc",
  sha256 = "67412176974bfce3f4cf8bdaff39784a72ed709fc58def599d1f68710b58d68b",
  strip_prefix = "rules_cc-b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e",
  urls = [
    "https://mirror.bazel.build/github.com/bazelbuild/rules_cc/archive/b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e.zip",
    "https://github.com/bazelbuild/rules_cc/archive/b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e.zip",
  ],
)

http_archive(
    name = "io_bazel_rules_go",
    urls = ["https://github.com/bazelbuild/rules_go/releases/download/0.19.1/rules_go-0.19.1.tar.gz"],
    sha256 = "8df59f11fb697743cbb3f26cfb8750395f30471e9eabde0d174c3aebc7a1cd39",
)
load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies", "go_register_toolchains")
go_rules_dependencies()
go_register_toolchains()

http_archive(
    name = "bazel_gazelle",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/releases/download/0.17.0/bazel-gazelle-0.17.0.tar.gz"],
    sha256 = "3c681998538231a2d24d0c07ed5a7658cb72bfb5fd4bf9911157c0e9ac6a2687",
)
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")
gazelle_dependencies()

######################################################
# haskell

http_archive(
  name = "rules_haskell",
  strip_prefix = "rules_haskell-master",
  urls = ["https://github.com/pompon0/rules_haskell/archive/master.tar.gz"],
  sha256 = "7690ae05d794f98724bd353ffc8600fd76b578db858818eb6645a7680708d22d",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies", "rules_haskell_toolchains")
rules_haskell_dependencies()
rules_haskell_toolchains()

http_archive(
  name = "happy",
  strip_prefix = "happy-1.19.10",
  urls = ["http://hackage.haskell.org/package/happy-1.19.10/happy-1.19.10.tar.gz"],
  sha256 = "22eb606c97105b396e1c7dc27e120ca02025a87f3e44d2ea52be6a653a52caed",
  build_file = "//:third_party/happy.BUILD",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
  name = "stackage",
  packages = [
    "base",
    "data-default-class",
    "haskell-src-exts",
    "either",
    "tasty-hunit",
    "tasty",
    "parsec",
    "transformers",
    "unix",
    "bytestring",
    "utf8-string",
    "tar",
    "http-conduit",
    "zlib",
    "lens",
    "proto-lens-runtime",
    "proto-lens",
    "microlens",
    "lens-family",
    "text",
    "containers",
    "mtl",
    "MissingH",
    "threads",
    "concurrent-extra",
    "unbounded-delays",
    "deepseq",
    "split",
    "data-ordlist",
    "clock",
    "hashable",
    "hashtables",
    "options",
    "array",
    "vector",
    "proto-lens-protoc",
  ],
  snapshot = "lts-14.2",
  tools = ["@happy"],
)

http_archive(
  name = "set-monad",
  strip_prefix = "set-monad-master",
  urls = ["https://github.com/giorgidze/set-monad/archive/master.zip"],
  sha256 = "64079a9dd5d59b92ceaf7c79768ccbcb753c9bc4a9935cfb8e430519d32ca468",
  build_file = "//:third_party/set-monad.BUILD",
)

http_archive(
  name = "proto-lens-protoc",
  urls = ["http://hackage.haskell.org/package/proto-lens-protoc-0.5.0.0/proto-lens-protoc-0.5.0.0.tar.gz"],
  build_file = "//:third_party/proto-lens-protoc.BUILD",
  sha256 = "161dcee2aed780f62c01522c86afce61721cf89c0143f157efefb1bd1fa1d164",
  strip_prefix = "proto-lens-protoc-0.5.0.0",
)

register_toolchains(":protobuf-toolchain")

######################################################
# generated by gazelle

go_repository(
    name = "org_golang_x_sync",
    commit = "e225da77a7e68af35c70ccbf71af2b83e6acac3c",
    importpath = "golang.org/x/sync",
)
