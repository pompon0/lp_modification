#include <iostream>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/parse2.h"

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);

  return 0;
}
