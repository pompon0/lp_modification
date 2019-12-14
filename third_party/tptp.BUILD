load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")

haskell_cabal_library(
  name = "tptp",
  version = "0.1.1.0",
  srcs = glob(["**","tptp.cabal"]),
  visibility = ["//visibility:public"],
  deps = [
    "@stackage//:base",
    "@stackage//:attoparsec",
    "@stackage//:prettyprinter",
    "@stackage//:scientific",
    "@stackage//:semigroups",
    "@stackage//:text",
  ],
)
