package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "syntax",
    hdrs = [
        "atom.h",
        "clause.h",
        "show.h",
        "term.h",
    ],
    deps = [
        "//lazyparam_prover/memory",
    ],
)

cc_test(
    name = "term_test",
    srcs = ["term_test.cc"],
    deps = [
        ":syntax",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "clause_test",
    srcs = ["clause_test.cc"],
    deps = [
        ":syntax",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "atom_test",
    srcs = ["atom_test.cc"],
    deps = [
        ":syntax",
        "@gtest//:gtest_main",
    ],
)
