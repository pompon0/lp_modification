load("@rules_haskell//haskell:defs.bzl", "haskell_binary", "haskell_library", "haskell_test")

haskell_library(
    name = "Err",
    srcs = ["Err.hs"],
    deps = [
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:transformers",
    ],
)

haskell_test(
    name = "Err_test",
    srcs = ["ErrTest.hs"],
    deps = [
        ":Err",
        "@stackage//:base",
        "@stackage//:lens",
        "@stackage//:tasty",
        "@stackage//:tasty-hunit",
    ],
)

haskell_library(
    name = "Tptp",
    srcs = [
        "Ctx.hs",
        "Err.hs",
        "HashSeq.hs",
        "Parser.hs",
        "Tptp.hs",
    ],
    deps = [
        ":Err",
        "//tptp_parser/proto:tptp_haskell_proto",
        "@stackage//:attoparsec",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:parsec",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:transformers",
        "@tptp",
    ],
)

haskell_library(
    name = "Pred",
    srcs = [
        "Coq.hs",
        "Pred.hs",
    ],
    deps = [
        ":Tptp",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
    ],
)

haskell_library(
    name = "MGU",
    srcs = ["MGU.hs"],
    deps = [
        ":Pred",
        ":Tptp",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:mtl",
        "@stackage//:transformers",
    ],
)

haskell_test(
    name = "MGU_test",
    srcs = ["MGUTest.hs"],
    deps = [
        ":IO",
        ":MGU",
        ":Pred",
        ":Tptp",
        "//tptp_parser/proto:tptp_haskell_proto",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:tasty",
        "@stackage//:tasty-hunit",
        "@stackage//:text",
    ],
)

haskell_library(
    name = "FOF",
    srcs = [
        "FOF.hs",
    ],
    deps = [
        ":Pred",
        ":Tptp",
        "//tptp_parser/proto:tptp_haskell_proto",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:transformers",
    ],
)

haskell_library(
    name = "DNF",
    srcs = [
        "DNF.hs",
    ],
    deps = [
        ":FOF",
        ":MGU",
        ":Pred",
        ":Tptp",
        "//tptp_parser/proto:tptp_haskell_proto",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:text",
    ],
)

haskell_library(
    name = "Valid",
    srcs = [
        "EqAxioms.hs",
        "Proof.hs",
        "Valid.hs",
    ],
    deps = [
        ":DNF",
        ":Pred",
        ":Tptp",
        "//tptp_parser/proto:solutions_haskell_proto",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:transformers",
    ],
)

haskell_library(
    name = "IO",
    srcs = ["IO.hs"],
    deps = [
        "@stackage//:base",
    ],
)

haskell_binary(
    name = "tool",
    srcs = ["ToolBin.hs"],
    features = ["fully_static_link"],
    visibility = ["//visibility:public"],
    deps = [
        ":DNF",
        ":FOF",
        ":IO",
        ":Tptp",
        ":Valid",
        "//tptp_parser/proto:solutions_haskell_proto",
        "//tptp_parser/proto:tptp_haskell_proto",
        "@stackage//:base",
        "@stackage//:bytestring",
        "@stackage//:containers",
        "@stackage//:deepseq",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:text",
    ],
)
