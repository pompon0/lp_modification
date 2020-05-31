package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "memory",
    hdrs = [
        "alloc.h",
        "array.h",
        "layout.h",
        "lazy.h",
        "list.h",
        "maybe.h",
        "variant.h",
    ],
    srcs = [
        "alloc.cc",
    ],
    deps = [
        "//lazyparam_prover:log",
        "//lazyparam_prover:types",
    ],
)

cc_test(
    name = "alloc_test",
    srcs = ["alloc_test.cc"],
    deps = [
        ":memory",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)
