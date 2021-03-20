package(
  default_visibility = ["//visibility:public"],
)

cc_library(
  name = "dmlc",
  includes = [
      "src/",
      "include/",
  ],
  hdrs = glob([
    "src/**/*.h",
    "include/**/*.h",
  ]),
  srcs = glob([
    "src/**/*.cc",
  ],exclude=[
    "src/io/azure_filesys.cc",
    "src/io/hdfs_filesys.cc",
  ]),
  linkopts = [
    "-lcurl",
    "-lcrypto",
  ],
)

