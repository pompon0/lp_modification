package(
  default_visibility = ["//visibility:public"],
)

cc_library(
  name = "xgboost",
  includes = [
      "include/",
      "src/",
  ],
  hdrs = glob([
    "include/**/*.h",
    "src/**/*.h",
    "src/**/*.cu",
  ]),
  srcs = glob([
    "src/**/*.cc",
  ], exclude = [
    "src/cli_main.cc",
  ]),
  copts = [
    "-fopenmp",
    "--std=c++17",
  ],
  deps = [
    "@rabit//:rabit",
    "@dmlc//:dmlc",
    "@openmp//:openmp",
  ],
)

