load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_test", "haskell_binary")

haskell_library(
    name = "Ctx",
    srcs = [
        "Ctx.hs",
        "HashSeq.hs",
    ],
    deps = [
        "@stackage//:base",
        "@stackage//:lens",
        "@stackage//:containers",
    ],
)

haskell_library(
    name = "Tptp",
    srcs = [
        "Tptp.hs",
        "Parser.hs",
    ],
    deps = [
        ":Ctx",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:parsec",
        "@stackage//:text",
        "@stackage//:attoparsec",
        "@tptp//:tptp",
        "//tptp_parser/proto:tptp_haskell_proto",
    ],
)

haskell_library(
    name = "Pred",
    srcs = [
        "Pred.hs",
        "Coq.hs",
    ],
    deps = [
        ":Ctx",
        "@stackage//:base",
        "@stackage//:lens",
        "@stackage//:containers",
    ],
)

haskell_library(
    name = "MGU",
    srcs = ["MGU.hs"],
    deps = [
        ":Ctx",
        ":Pred",
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
        ":Ctx",
        ":MGU",
        ":Pred",
        ":IO",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:tasty",
        "@stackage//:tasty-hunit",
    ],
)

haskell_library(
    name = "FOF",
    srcs = [
        "FOF.hs",
        "NNF.hs",
    ],
    deps = [
        ":Ctx",
        ":Tptp",
        ":Pred",
        "@stackage//:base",
        "@stackage//:text",
        "@stackage//:lens",
        "@stackage//:transformers",
        "@stackage//:proto-lens",
        "@stackage//:containers",
        "//tptp_parser/proto:tptp_haskell_proto",
    ],
)

haskell_library(
    name = "DNF",
    srcs = [
        "DNF.hs",
        "DefDNF.hs",
    ],
    deps = [
        ":Ctx",
        ":Tptp",
        ":Pred",
        ":FOF",
        ":MGU",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:text",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "//tptp_parser/proto:tptp_haskell_proto",
    ],
)

haskell_test(
    name = "DefDNF_test",
    srcs = ["DefDNFTest.hs"],
    deps = [
        ":Ctx",
        ":Pred",
        ":FOF",
        ":DNF",
        ":Tptp",
        "@stackage//:lens",
        "@stackage//:base",
        "@stackage//:tasty",
        "@stackage//:tasty-hunit",
    ],
)

haskell_library(
    name = "Valid",
    srcs = [
        "Valid.hs",
        "Proof.hs",
        "EqAxioms.hs",
    ],
    deps = [
        ":Ctx",
        ":DNF",
        ":Pred",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:transformers",
        "//tptp_parser/proto:solutions_haskell_proto",
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
    deps = [
        ":Ctx",
        ":DNF",
        ":FOF",
        ":Tptp",
        ":Valid",
        ":IO",
        "@stackage//:base",
        "@stackage//:bytestring",
        "@stackage//:containers",
        "@stackage//:lens",
        "@stackage//:proto-lens",
        "@stackage//:text",
        "@stackage//:deepseq",
        "//tptp_parser/proto:tptp_haskell_proto",
        "//tptp_parser/proto:solutions_haskell_proto",
    ],
    features = ["fully_static_link"],
    visibility = ["//visibility:public"],
)
