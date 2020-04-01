// TODO: static linking of absl doesn't work reliably right now
// reenable once that's fixed.
// #include "absl/flags/flag.h"
// #include "absl/flags/parse.h"
#include "lazyparam_prover/parse2.h"

#include <iostream>
#include "lazyparam_prover/log.h"
#include "tptp.pb.h"

using namespace tableau;

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  //absl::ParseCommandLine(argc, argv);

  tptp::ToolOutput out;
  parse_file(out.mutable_file(),std::cin);
  out.set_has_equality(has_equality(out.file()));
  out.SerializeToOstream(&std::cout);
  //inline_imports(std::cout,std::cin);
  std::cout << std::flush;
  return 0;
}
