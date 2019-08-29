load("@bazel_gazelle//:def.bzl", "gazelle")

# gazelle:prefix github.com/pompon0/tptp_benchmark_go
# gazelle:build_file_name BUILD
# gazelle:proto disable
gazelle(name = "gazelle")

### haskell proto toolchain
load("@rules_haskell//haskell:protobuf.bzl", "haskell_proto_toolchain")

haskell_proto_toolchain(
    name = "protobuf-toolchain",
    protoc = "@com_google_protobuf//:protoc",
    plugin = "@proto-lens-protoc//:proto-lens-protoc",
    deps = [
        "@stackage//:base",
        "@stackage//:bytestring",
        "@stackage//:containers",
        "@stackage//:data-default-class",
        "@stackage//:lens-family",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:deepseq",
        "@stackage//:vector",
    ],
)
