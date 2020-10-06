package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "engine",
    hdrs = [
        "engine.h",
        "balanced.h",
        "depth.h",
    ],
    deps = [
        "//utils:utils",
        "//tptp_parser/proto:tptp_cc_proto",
        "//tptp_parser/proto:solutions_cc_proto",
    ],
)
