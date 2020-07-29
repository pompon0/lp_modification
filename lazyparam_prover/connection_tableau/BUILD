package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "prover",
    hdrs = [
        "cont.h",
        "tableau.h",
    ],
    deps = [
        "//lazyparam_prover/connection_tableau/frames:frames",
        "//tptp_parser/proto:tptp_cc_proto",
        "//tptp_parser/proto:solutions_cc_proto",
        "//lazyparam_prover:log",
        "//lazyparam_prover:parse",
        "//lazyparam_prover/syntax:syntax",
        "//lazyparam_prover:types",
        "//lazyparam_prover:mgu",
        "//lazyparam_prover/util:util",
    ],
)
