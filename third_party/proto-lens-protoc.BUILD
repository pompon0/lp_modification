load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")

haskell_cabal_binary(
  name = "proto-lens-protoc",
  srcs = glob(["**"]),
  deps = [
    "@stackage//:base",
    "@stackage//:bytestring",
    "@stackage//:containers",
    "@stackage//:lens-family",
    "@stackage//:proto-lens",
    "@stackage//:proto-lens-protoc",
    "@stackage//:text",
  ],
  visibility = ["//visibility:public"],
)
