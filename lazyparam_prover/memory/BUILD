package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "memory",
    hdrs = [
        "array.h",
        "function.h",
        "layout.h",
        "lazy.h",
        "list.h",
        "maybe.h",
        "stack.h",
    ],
    deps = [
        "//utils",
    ],
)

cc_test(
    name = "alloc_test",
    srcs = ["alloc_test.cc"],
    deps = [
        ":memory",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "stack_test",
    srcs = ["stack_test.cc"],
    deps = [
        ":memory",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "list_test",
    srcs = ["list_test.cc"],
    deps = [
        ":memory",
        "//utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "layout_test",
    srcs = ["layout_test.cc"],
    deps = [
        ":memory",
        "//utils",
        "@gtest//:gtest_main",
    ],
)
