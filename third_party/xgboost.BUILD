load("@rules_foreign_cc//tools/build_defs:cmake.bzl", "cmake_external")

filegroup(
  name = "all",
  srcs = glob(["**"]),
)

cmake_external(
  name = "xgboost",
  lib_source = ":all",
  cache_entries = { "BUILD_STATIC_LIB": "ON" },
  static_libraries = [
    "libxgboost.a",
    "librabit.a",
    "libdmlc.a",
  ],
  visibility = ["//visibility:public"],
)
