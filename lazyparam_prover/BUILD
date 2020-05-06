load("@io_bazel_rules_go//proto:def.bzl", "go_proto_library")

package(
    default_visibility = ["//visibility:public"],
)

proto_library(
    name = "prover_proto",
    srcs = ["prover.proto"],
)

cc_proto_library(
    name = "prover_cc_proto",
    deps = [":prover_proto"],
)

go_proto_library(
    name = "prover_go_proto",
    importpath = "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/prover_go_proto",
    proto = ":prover_proto",
)

cc_library(
    name = "enum_flag",
    hdrs = ["enum_flag.h"],
    deps = [
        "@abseil//absl/strings:strings",
        "@com_google_protobuf//:protobuf",
        "//lazyparam_prover/util:util",
    ],
)

cc_library(
    name = "types",
    hdrs = ["types.h"],
)

cc_library(
    name = "log",
    hdrs = ["log.h"],
    srcs = ["log.cc"],
    deps = [
        ":types",
        "//lazyparam_prover/util:util",
    ],
)

cc_library(
    name = "derived",
    hdrs = [
        "constraint.h",
        "derived.h",
    ],
    deps = [
        ":log",
        ":types",
        "//lazyparam_prover/memory:memory",
        "//lazyparam_prover/syntax:syntax",
    ],
)

cc_test(
    name = "derived_test",
    srcs = [
      "derived_test.cc",
      "constraint_test.cc",
    ],
    deps = [
        ":derived",
        ":log",
        "@gtest//:gtest_main",
    ],
)

cc_library(
    name = "parse",
    hdrs = [
        "parse.h",
    ],
    deps = [
        ":types",
        "//lazyparam_prover/syntax:syntax",
        "//tptp_parser/proto:tptp_cc_proto",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_library(
    name = "parse2",
    hdrs = [
        "parse2.h",
    ],
    copts = ["-std=c++11"],
    deps = [
        ":types",
        "//lazyparam_prover/syntax:syntax",
        "//tptp_parser/proto:tptp_cc_proto",
        # vampire agressively takes over signal handling of the whole binary
        # in particular it severly reduces the global memory limits.
        # DO NOT LINK into actual prover, until you refector it.
        "@vampire//:lib",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_binary(
    name = "tool",
    srcs = ["tool.cc"],
    copts = ["-std=c++11"],
    deps = [
        "//tptp_parser/proto:tptp_cc_proto",
        ":parse2",
        ":log",
        # "@abseil//absl/flags:parse",
        # "@abseil//absl/flags:flag",
    ],
    visibility = ["//visibility:public"],
    features = ["fully_static_link"],
    linkopts = [
        "-static-libstdc++",
        "-static-libgcc",
        "-l:libstdc++.a",
    ],
)

cc_library(
    name = "mgu",
    hdrs = [
        "mgu.h",
        "kbo.h",
        "lpo.h",
        "constraint.h",
        "constrained_valuation.h",
        "rewrite_test_utils.h",
    ],
    deps = [
        ":derived",
        "//lazyparam_prover/syntax:syntax",
        "//lazyparam_prover/memory:memory",
    ],
)

cc_library(
    name = "prover",
    hdrs = [
        "ctx.h",
        "alt.h",
        "eq_axioms.h",
        "ground.h",
        "tableau.h",
        "lazy.h",
        "index.h",
        "prover_output.h",
    ],
    deps = [
        "//tptp_parser/proto:tptp_cc_proto",
        "//tptp_parser/proto:solutions_cc_proto",
        ":log",
        ":parse",
        "//lazyparam_prover/syntax:syntax",
        ":types",
        ":mgu",
        "//lazyparam_prover/util:util",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_test(
    name = "mgu_test",
    srcs = ["mgu_test.cc"],
    deps = [
        ":mgu",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "kbo_test",
    srcs = ["kbo_test.cc"],
    deps = [
        ":mgu",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "lpo_test",
    srcs = ["lpo_test.cc"],
    deps = [
        ":mgu",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)

cc_test(
    name = "eq_axioms_test",
    srcs = ["eq_axioms_test.cc"],
    deps = [
        ":prover",
        "//lazyparam_prover/util:util",
        "@gtest//:gtest_main",
    ],
)

cc_binary(
    name = "main",
    srcs = ["main.cc"],
    deps = [
        ":enum_flag",
        ":prover",
        ":prover_cc_proto",
        "@abseil//absl/flags:flag",
        "@abseil//absl/flags:parse",
    ],
    features = ["fully_static_link"],
    linkopts = [
        "-static-libstdc++",
        "-static-libgcc",
        "-l:libstdc++.a",
    ],
    visibility = ["//visibility:public"],
)
