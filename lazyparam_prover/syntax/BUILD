package(
  default_visibility = ["//visibility:public"],
)

cc_library(
  name = "syntax",
  hdrs = [
    "term.h",
    "atom.h",
    "clause.h",
    "show.h",
  ],
  deps = [
      "//lazyparam_prover/memory:memory",
  ],
)

cc_test(
  name = "clause_test",
  srcs = ["clause_test.cpp"],
  deps = [
    ":syntax",
    "//lazyparam_prover:types",
    "//lazyparam_prover/util:util",
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
