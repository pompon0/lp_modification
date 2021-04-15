load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

################################
# BAZEL

# "com_google_protobuf" declares some terribly old version of skylib,
# forcing the newest version beforehead
http_archive(
    name = "bazel_skylib",
    sha256 = "e5d90f0ec952883d56747b7604e2a15ee36e288bb556c3d0ed33e818a4d971f2",
    strip_prefix = "bazel-skylib-1.0.2",
    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/1.0.2.tar.gz"],
)

###############################
# LLVM

http_archive(
  # cannot change this name, because files in its workspace refer to it
  name = "com_grail_bazel_toolchain",
  strip_prefix = "bazel-toolchain-e608913c0e106da931234fdd37de9ee37e0b2541",
  urls = ["https://github.com/pompon0/bazel-toolchain/archive/e608913c0e106da931234fdd37de9ee37e0b2541.tar.gz"],
  sha256 = "e09e8ef8a5f97078da2961561e176a5bf461962683159bcbd81674052475cdd0",
)

load("@com_grail_bazel_toolchain//toolchain:deps.bzl", "bazel_toolchain_dependencies")

bazel_toolchain_dependencies()

load("@com_grail_bazel_toolchain//toolchain:rules.bzl", "llvm_toolchain")

llvm_toolchain(
  name = "llvm_toolchain",
  distribution = "clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz",
  llvm_version = "10.0.0",
)

load("@llvm_toolchain//:toolchains.bzl", "llvm_register_toolchains")

llvm_register_toolchains()

################################
# GOLANG

http_archive(
    name = "io_bazel_rules_go",
    urls = ["https://github.com/bazelbuild/rules_go/releases/download/v0.23.3/rules_go-v0.23.3.tar.gz"],
    sha256 = "a8d6b1b354d371a646d2f7927319974e0f9e52f73a2452d2b3877118169eb6bb",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies", "go_register_toolchains")
go_rules_dependencies()
go_register_toolchains()

http_archive(
    name = "bazel_gazelle",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.19.1/bazel-gazelle-v0.19.1.tar.gz"],
    sha256 = "86c6d481b3f7aedc1d60c1c211c6f76da282ae197c3b3160f54bd3a8f847896f",
)
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")
gazelle_dependencies()
# gazelle:repository_macro gazelle_generated_rules.bzl%gazelle_generated
load("//:gazelle_generated_rules.bzl","gazelle_generated")
gazelle_generated()

################################
# PROTO

http_archive(
    name = "rules_proto",
    # sha256 = "602e7161d9195e50246177e7c55b2f39950a9cf7366f74ed5f22fd45750cd208",
    sha256 = "9fc210a34f0f9e7cc31598d109b5d069ef44911a82f507d5a88716db171615a8",
    strip_prefix = "rules_proto-f7a30f6f80006b591fa7c437fe5a951eb10bcbcf",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/f7a30f6f80006b591fa7c437fe5a951eb10bcbcf.tar.gz",
        "https://github.com/bazelbuild/rules_proto/archive/f7a30f6f80006b591fa7c437fe5a951eb10bcbcf.tar.gz",
    ],
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

################################
# C++

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

################################
# DOCKER

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "7eff487e95d268d577f13f6e3ea49be8704504b2096367e4a2181ecbbc111bad",
    strip_prefix = "rules_docker-9bfcd7dbf0294ed9d11a99da6363fc28df904502",
    urls = ["https://github.com/bazelbuild/rules_docker/archive/9bfcd7dbf0294ed9d11a99da6363fc28df904502.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)

container_repositories()

load("@io_bazel_rules_docker//go:image.bzl", _go_image_repos = "repositories")
_go_image_repos()

load("@io_bazel_rules_docker//container:container.bzl", "container_pull")

container_pull(
    name = "swipl",
    registry = "index.docker.io",
    repository = "amd64/swipl",
    digest = "sha256:b6f51e9cbccc55386bfa3381336c70bda145405cc258055fc8cc2afaddb9a35b",
)

################################
# HASKELL

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-51f854748cd634b30001c49b6fe4c1574c0a3da0",
    urls = ["https://github.com/pompon0/rules_haskell/archive/51f854748cd634b30001c49b6fe4c1574c0a3da0.tar.gz"],
    sha256 = "75b2b48ec147cc162a22538466e876d83f0936281a188119ccce5a83c60bc934",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains()

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "attoparsec",
        "base",
        "bytestring",
        "containers",
        "data-default-class",
        "deepseq",
        "haskell-src-exts",
        "lens",
        "lens-family",
        "mtl",
        "parsec",
        "prettyprinter",
        "proto-lens",
        "proto-lens-protoc",
        "proto-lens-runtime",
        "scientific",
        "semigroups",
        "tasty",
        "tasty-hunit",
        "text",
        "transformers",
        "vector",
    ],
    snapshot = "lts-14.2",
)

http_archive(
    name = "tptp",
    urls = ["https://github.com/aztek/tptp/archive/c920f1e611db53062fccc1bc5b7b3e2c0537cd15.zip"],
    build_file = "//:third_party/tptp.BUILD",
    sha256 = "f6d9f0a9e39e725c46ff94ceac20e8b689ec80184d612d1e2fb60c8f4876ea49",
)

http_archive(
    name = "proto-lens-protoc",
    urls = ["http://hackage.haskell.org/package/proto-lens-protoc-0.5.0.0/proto-lens-protoc-0.5.0.0.tar.gz"],
    build_file = "//:third_party/proto-lens-protoc.BUILD",
    sha256 = "161dcee2aed780f62c01522c86afce61721cf89c0143f157efefb1bd1fa1d164",
    strip_prefix = "proto-lens-protoc-0.5.0.0",
)

register_toolchains(":protobuf-toolchain")

################################
# CMAKE

# Rule repository, note that it's recommended to use a pinned commit to a released version of the rules
http_archive(
  name = "rules_foreign_cc",
  sha256 = "c2cdcf55ffaf49366725639e45dedd449b8c3fe22b54e31625eb80ce3a240f1e",
  strip_prefix = "rules_foreign_cc-0.1.0",
  url = "https://github.com/bazelbuild/rules_foreign_cc/archive/0.1.0.zip",
)

load("@rules_foreign_cc//:workspace_definitions.bzl", "rules_foreign_cc_dependencies")

rules_foreign_cc_dependencies()

################################
# XGBOOST

http_archive(
  name = "openmp",
  strip_prefix = "llvm-project-llvmorg-11.0.0",
  url = "https://github.com/llvm/llvm-project/archive/llvmorg-11.0.0.zip",
  sha256 = "6db05aa39192950b73311672c074b3942aedbb92d5ff14e170323fc747ccb99f",
  build_file = "//:third_party/openmp.BUILD",
)

http_archive(
  name = "dmlc",
  strip_prefix = "dmlc-core-5df8305fe699d3b503d10c60a231ab0223142407",
  url = "https://github.com/dmlc/dmlc-core/archive/5df8305fe699d3b503d10c60a231ab0223142407.zip",
  sha256 = "10a0c81866dbd2e4bd23da031ec44e05e56e6cd75506dd9b37cd64e83169c99c",
  build_file = "//:third_party/dmlc.BUILD",
)

http_archive(
  name = "rabit",
  strip_prefix = "rabit-4fb34a008db6437c84d1877635064e09a55c8553",
  url = "https://github.com/dmlc/rabit/archive/4fb34a008db6437c84d1877635064e09a55c8553.zip",
  sha256 = "9928f637c5d6d6f44512f95eb6ffa7ed878eb9b300a897514abc38bba3759dce",
  build_file = "//:third_party/rabit.BUILD",
)

http_archive(
  name = "xgboost",
  strip_prefix = "xgboost-34408a7fdcebc0e32142ed2f52156ea65d813400",
  url = "https://github.com/dmlc/xgboost/archive/34408a7fdcebc0e32142ed2f52156ea65d813400.zip",
  sha256 = "8baee6a5f4198803cf2623c4d1c3c1326f026309b289bc663c3e492a7f30d34a",
  build_file = "//:third_party/xgboost.BUILD",
)

################################
# OTHER

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
    name = "vampire",
    strip_prefix = "vampire-ec2c30cb615e41aecd04be6b02d4718e22e4774e",
    urls = ["https://github.com/pompon0/vampire/archive/ec2c30cb615e41aecd04be6b02d4718e22e4774e.tar.gz"],
    sha256 = "b9f1248f9334df237b8cacbec76f3590ff18ea47b7efa9c2c431b25f9e4b1dac",
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

http_file(
    name = "tptp4X",
    executable = 1,
    urls = ["https://storage.googleapis.com/tptp/tptp4X"],
    sha256 = "2418cb42c0f9013289ef7653c4ad9cd4d9b7d7ac67bbf5687dbb1ef1639e590e",
)

http_file(
    name = "mizar_problems",
    urls = ["https://storage.googleapis.com/tptp/mizar_problems.zip"],
    sha256 = "70a6e8467753395125f281ea371adc390585b667b84db79451fd0cc8780bd749",
)

http_file(
    name = "tptp_problems",
    urls = ["https://storage.googleapis.com/tptp/tptp_problems_long.zip"],
    sha256 = "7872e2032bdd06aa7857915305ec70663a17c3346f84bb50bf5155986d8c2c00",
)

