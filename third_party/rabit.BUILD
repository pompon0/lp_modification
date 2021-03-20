package(
  default_visibility = ["//visibility:public"],
)

cc_library(
  name = "rabit",
  includes = [
      "include/",
      "src/",
  ],
  hdrs = glob([
    "include/**/*.h",
    "src/**/*.h",
  ]),
  srcs = glob([
    "src/**/*.cc",
  ],exclude=[
    "src/engine_base.cc",
    "src/engine_mpi.cc",
    "src/engine_mock.cc",
    "src/engine_empty.cc",
  ]),
  deps = [
    "@dmlc//:dmlc",
  ],
)

