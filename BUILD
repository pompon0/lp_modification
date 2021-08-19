load("@bazel_gazelle//:def.bzl", "gazelle")

# gazelle:prefix github.com/pompon0/tptp_benchmark_go
# gazelle:build_file_name BUILD
# gazelle:proto disable
# gazelle:go_naming_convention go_default_library
gazelle(name = "gazelle")

### haskell proto toolchain
load("@rules_haskell//haskell:protobuf.bzl", "haskell_proto_toolchain")

haskell_proto_toolchain(
    name = "protobuf-toolchain",
    plugin = "@proto-lens-protoc//:proto-lens-protoc",
    protoc = "@com_google_protobuf//:protoc",
    deps = [
        "@stackage//:base",
        "@stackage//:bytestring",
        "@stackage//:containers",
        "@stackage//:data-default-class",
        "@stackage//:deepseq",
        "@stackage//:lens-family",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:vector",
    ],
)
