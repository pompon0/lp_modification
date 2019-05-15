COPTS = [
	"-std=gnu99",
	"-O3",
	"-fomit-frame-pointer",
	"-fno-common",
	"-Wall",
]

cc_library(
  name = "libs",
  includes = [
  	"E/include/",
    "E/BASICS/",
    "E/TERMS/",
    "E/CLAUSES/",
    "E/INOUT/",
    "E/LEARN/",
    "E/CONTROL/",
    "E/HEURISTICS/",
    "E/ORDERINGS/",
  ],
  srcs = glob([
    "E/CONTRIB/picosat-965/picosat.c",
    "E/BASICS/*.c",
    "E/TERMS/*.c",
    "E/CLAUSES/*.c",
    "E/INOUT/*.c",
    "E/LEARN/*.c",
    "E/CONTROL/*.c",
    "E/HEURISTICS/*.c",
    "E/ORDERINGS/*.c",
  ], exclude = [
    "E/BASICS/clb_newmem.c",
    "E/TERMS/cte_functypes.c",
    "E/HEURISTICS/che_auto_cases.c",
    "E/HEURISTICS/che_X_*.c",
  ]),
  hdrs = glob([
		"E/include/*.h",
    "E/CONTRIB/picosat-965/picosat.h",
    "E/BASICS/*.h",
    "E/TERMS/*.h",
    "E/TERMS/cte_functypes.c",
    "E/CLAUSES/*.h",
    "E/INOUT/*.h",
    "E/LEARN/*.h",
    "E/CONTROL/*.h",
    "E/HEURISTICS/*.h",
    "E/HEURISTICS/che_auto_cases.c",
    "E/HEURISTICS/che_X_*.c",
    "E/ORDERINGS/*.h",
  ]),
  defines = [
		"PRINT_SOMEERRORS_STDOUT",
		"MEMORY_RESERVE_PARANOID",
		"PRINT_TSTP_STATUS",
		"STACK_SIZE=32768",
		"CLAUSE_PERM_IDENT",
		"TAGGED_POINTERS",
		"NDEBUG",
    "FAST_EXIT",
  ],
	copts = COPTS, 
)

cc_library(
  name = "prover_headers",
  includes = ["E/PROVER/"],
  hdrs = glob(["E/PROVER/*.h"]),
)

cc_binary(
  name = "prover_bin",
  srcs = ["E/PROVER/eprover.c"],
	copts = COPTS,
	deps = [":libs"],
)
