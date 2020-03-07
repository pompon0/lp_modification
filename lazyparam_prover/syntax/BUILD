package(
  default_visibility = ["//visibility:public"],
)

cc_library(
  name = "syntax",
  hdrs = [
    "term.h",
    "atom.h",
    "clause.h",
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
