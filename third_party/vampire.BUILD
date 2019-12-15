COPTS = [
  "-O6",
  "-std=c++11",
  "-Wno-return-type",
  "-Wno-terminate",
  "-Wno-maybe-uninitialized",
  "-Wno-switch",
  "-Wno-unused-value",
  "-Wno-unused-variable",
  "-Wno-unused-but-set-variable",
  "-Wno-uninitialized",
  "-Wno-narrowing",
  "-Wno-strict-aliasing",
]

DEFINES = [
  "VDEBUG=0",
  "GNUMP=0",
  "VZ3=0",
  "__STDC_LIMIT_MACROS",
  "__STDC_FORMAT_MACROS",
]

cc_library(
  name = "vampire_lib",
  hdrs = glob([
      "**/*.hpp",
      "**/*.h",
    ], exclude=[
      "z3/**/*",
    ]
  ),
  srcs = glob([
      "vampire.cpp",
      "**/*.cpp",
      "**/*.cc",
    ], exclude = [
      "z3/**/*",
      "Test/**/*",
      "UnitTests/**/*",
      "Api/Problem.cpp",
      "Api/FormulaBuilder.cpp",
      "Api/Helper.cpp",
      "Api/Helper_Internal.cpp",
      "Indexing/FormulaIndex.cpp",
      "Inferences/SLQueryForwardSubsumption.cpp",
      "Inferences/ArrayTheoryISE.cpp",
      "Inferences/RefutationSeekerFSE.cpp",
      "Parse/SMTLIB.cpp",
      "SAT/TransparentSolver.cpp",
      "SAT/SingleWatchSAT.cpp",
      "Shell/LTB/**/*", 
      "Shell/EqualityAxiomatizer.cpp", 
      "Shell/Profile.cpp",
      "Shell/ProofSimplifier.cpp",
      "VUtils/**/*",
      "vground.cpp",
      "vclausify.cpp",
      "vsat.cpp",
      "vutil.cpp",
      "vcompit.cpp",
      "vtest.cpp",
      "vltb.cpp",
      "ucompit.cpp",
      "test_vapi.cpp",
    ]
  ),
  includes = ["."],
  copts = COPTS,
  defines = DEFINES,
  visibility = ["//visibility:public"],
)
