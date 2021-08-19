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
    sha256 = "e09e8ef8a5f97078da2961561e176a5bf461962683159bcbd81674052475cdd0",
    strip_prefix = "bazel-toolchain-e608913c0e106da931234fdd37de9ee37e0b2541",
    urls = ["https://github.com/pompon0/bazel-toolchain/archive/e608913c0e106da931234fdd37de9ee37e0b2541.tar.gz"],
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
    urls = ["https://github.com/bazelbuild/rules_go/releases/download/v0.28.0/rules_go-v0.28.0.zip"],
)



http_archive(
    name = "bazel_gazelle",
    sha256 = "62ca106be173579c0a167deb23358fdfe71ffa1e4cfdddf5582af26520f1c66f",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz"],
)


# gazelle:repository_macro gazelle_generated_rules.bzl%gazelle_generated
load("//:gazelle_generated_rules.bzl", "gazelle_generated")

# gazelle_generated() has to be before all the go/gazelle dependencies, so that
# the selected versions override the defaults.
gazelle_generated()

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()
go_register_toolchains(version = "1.16.5")
gazelle_dependencies()

################################
# PROTO

http_archive(
    name = "rules_proto",
    sha256 = "36476f17a78a4c495b9a9e70bd92d182e6e78db476d90c74bac1f5f19f0d6d04",
    strip_prefix = "rules_proto-fcad4680fee127dbd8344e6a961a28eef5820ef4",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/fcad4680fee127dbd8344e6a961a28eef5820ef4.tar.gz",
        "https://github.com/bazelbuild/rules_proto/archive/fcad4680fee127dbd8344e6a961a28eef5820ef4.tar.gz",
    ],
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")

rules_proto_dependencies()

rules_proto_toolchains()

################################
# C++

http_archive(
    name = "gtest",
    sha256 = "353571c2440176ded91c2de6d6cd88ddd41401d14692ec1f99e35d013feda55a",
    strip_prefix = "googletest-release-1.11.0",
    urls = ["https://github.com/google/googletest/archive/release-1.11.0.zip"],
)

http_archive(
    name = "abseil",
    sha256 = "743f879030960c80a0445310567ca99d8eae87468dd0e78fd0c8bfd46f429e37",
    strip_prefix = "abseil-cpp-d659fe54b35ab9b8e35c72e50a4b8814167d5a84",
    urls = ["https://github.com/abseil/abseil-cpp/archive/d659fe54b35ab9b8e35c72e50a4b8814167d5a84.zip"],
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
    digest = "sha256:b6f51e9cbccc55386bfa3381336c70bda145405cc258055fc8cc2afaddb9a35b",
    registry = "index.docker.io",
    repository = "amd64/swipl",
)

################################
# HASKELL

http_archive(
    name = "rules_haskell",
    sha256 = "adb108d07be3944b9cabb4c14a69ad3ff10082f4c2bd133061e1d6a9f934d539",
    strip_prefix = "rules_haskell-6bff155511917a8b462132600bb2c0ebfa076a30",
    urls = ["https://github.com/pompon0/rules_haskell/archive/6bff155511917a8b462132600bb2c0ebfa076a30.tar.gz"],
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
    build_file = "//:third_party/tptp.BUILD",
    sha256 = "f6d9f0a9e39e725c46ff94ceac20e8b689ec80184d612d1e2fb60c8f4876ea49",
    urls = ["https://github.com/aztek/tptp/archive/c920f1e611db53062fccc1bc5b7b3e2c0537cd15.zip"],
)

http_archive(
    name = "proto-lens-protoc",
    build_file = "//:third_party/proto-lens-protoc.BUILD",
    sha256 = "161dcee2aed780f62c01522c86afce61721cf89c0143f157efefb1bd1fa1d164",
    strip_prefix = "proto-lens-protoc-0.5.0.0",
    urls = ["http://hackage.haskell.org/package/proto-lens-protoc-0.5.0.0/proto-lens-protoc-0.5.0.0.tar.gz"],
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
    build_file = "//:third_party/openmp.BUILD",
    sha256 = "6db05aa39192950b73311672c074b3942aedbb92d5ff14e170323fc747ccb99f",
    strip_prefix = "llvm-project-llvmorg-11.0.0",
    url = "https://github.com/llvm/llvm-project/archive/llvmorg-11.0.0.zip",
)

http_archive(
    name = "dmlc",
    build_file = "//:third_party/dmlc.BUILD",
    sha256 = "10a0c81866dbd2e4bd23da031ec44e05e56e6cd75506dd9b37cd64e83169c99c",
    strip_prefix = "dmlc-core-5df8305fe699d3b503d10c60a231ab0223142407",
    url = "https://github.com/dmlc/dmlc-core/archive/5df8305fe699d3b503d10c60a231ab0223142407.zip",
)

http_archive(
    name = "rabit",
    build_file = "//:third_party/rabit.BUILD",
    sha256 = "9928f637c5d6d6f44512f95eb6ffa7ed878eb9b300a897514abc38bba3759dce",
    strip_prefix = "rabit-4fb34a008db6437c84d1877635064e09a55c8553",
    url = "https://github.com/dmlc/rabit/archive/4fb34a008db6437c84d1877635064e09a55c8553.zip",
)

http_archive(
    name = "xgboost",
    build_file = "//:third_party/xgboost.BUILD",
    sha256 = "8baee6a5f4198803cf2623c4d1c3c1326f026309b289bc663c3e492a7f30d34a",
    strip_prefix = "xgboost-34408a7fdcebc0e32142ed2f52156ea65d813400",
    url = "https://github.com/dmlc/xgboost/archive/34408a7fdcebc0e32142ed2f52156ea65d813400.zip",
)

################################
# OTHER

http_archive(
    name = "eprover",
    build_file = "//:third_party/eprover.BUILD",
    sha256 = "5366d2de77e6856250e26a967642389e81a6f823caedccaf5022a09242aceb96",
    urls = [
        "http://wwwlehre.dhbw-stuttgart.de/~sschulz/WORK/E_DOWNLOAD/V_2.3/E.tgz",
        "https://storage.googleapis.com/tptp/eprover_2_3.tgz",
    ],
)

http_archive(
    name = "vampire",
    sha256 = "b9f1248f9334df237b8cacbec76f3590ff18ea47b7efa9c2c431b25f9e4b1dac",
    strip_prefix = "vampire-ec2c30cb615e41aecd04be6b02d4718e22e4774e",
    urls = ["https://github.com/pompon0/vampire/archive/ec2c30cb615e41aecd04be6b02d4718e22e4774e.tar.gz"],
)

http_archive(
    name = "leancop",
    build_file = "//:third_party/leancop.BUILD",
    sha256 = "c8c154c7f694ffd5eee7453bb50bd07f5eac0b4c564a5b9a0cb548e802ed7bbf",
    urls = ["https://storage.googleapis.com/tptp/leancop_bin.tgz"],
)

http_archive(
    name = "leancop_prolog",
    build_file = "//:third_party/leancop_prolog.BUILD",
    sha256 = "ce432c5f9368c093f08df3120218463d8bdb8412e71ec980ab9c852c13cef300",
    urls = [
        "http://www.leancop.de/programs/leancop21.tar.gz",
        "https://storage.googleapis.com/tptp/leancop21.tar.gz",
    ],
)

http_file(
    name = "tptp4X",
    executable = 1,
    sha256 = "2418cb42c0f9013289ef7653c4ad9cd4d9b7d7ac67bbf5687dbb1ef1639e590e",
    urls = ["https://storage.googleapis.com/tptp/tptp4X"],
)

http_file(
    name = "mizar_problems",
    sha256 = "70a6e8467753395125f281ea371adc390585b667b84db79451fd0cc8780bd749",
    urls = ["https://storage.googleapis.com/tptp/mizar_problems.zip"],
)

http_file(
    name = "tptp_problems",
    sha256 = "7872e2032bdd06aa7857915305ec70663a17c3346f84bb50bf5155986d8c2c00",
    urls = ["https://storage.googleapis.com/tptp/tptp_problems_long.zip"],
)
