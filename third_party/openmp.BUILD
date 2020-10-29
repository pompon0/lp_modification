load("@rules_foreign_cc//tools/build_defs:cmake.bzl", "cmake_external")

filegroup(
  name = "openmp_src",
  srcs = glob(["openmp/**"]),
)

cmake_external(
  name = "openmp",
  lib_source = ":openmp_src",
  cache_entries = { "LIBOMP_ENABLE_SHARED": "OFF" },
  static_libraries = ["libomp.a"],
  visibility = ["//visibility:public"],
)
