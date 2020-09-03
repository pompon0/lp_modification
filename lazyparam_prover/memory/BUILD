package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "memory",
    hdrs = [
        "array.h",
        "layout.h",
        "lazy.h",
        "list.h",
        "maybe.h",
        "variant.h",
        "stack.h",
    ],
    deps = [
        "//utils:utils",
    ],
)

cc_test(
    name = "alloc_test",
    srcs = ["alloc_test.cc"],
    deps = [
        ":memory",
        "//utils:utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "stack_test",
    srcs = ["stack_test.cc"],
    deps = [
        ":memory",
        "//utils:utils",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "list_test",
    srcs = ["list_test.cc"],
    deps = [
        "//utils:utils",
        ":memory",
        "@gtest//:gtest_main",
    ],
)
