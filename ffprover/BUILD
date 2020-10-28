cc_library(
  name = "mcts_lib",
  hdrs = [
    "xgboost.h",
    "tree.h",
    "search.h",
  ],
  deps = [
    "//lazyparam_prover:controller",
    "//utils:utils",
    "@abseil//absl/flags:parse",
    "@abseil//absl/flags:flag",
    "@abseil//absl/time:time",
    "@xgboost//:xgboost",
  ],
)

cc_import(
  name = "openmp",
  static_library = "libomp.a",
)

cc_binary(
  name = "mcts",
  deps = [
    ":openmp",
    ":mcts_lib",
  ],
  srcs = ["mcts.cc"],
  features = ["fully_static_link"],
)
