#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "tool/conv.h"
#include "tool/node.h"

#include <iostream>
#include "utils/log.h"
#include "tptp.pb.h"

using namespace tool;

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);

  tptp::ToolOutput out;
  node::Index idx;
  conv::TptpToProto::file(idx,out.mutable_file(),std::cin);
  // This one cannot be just replaced with checking if
  // there is a PRED_EQ node, since it technically it can
  // be unused.
  out.set_has_equality(has_equality(out.file()));
  out.SerializeToOstream(&std::cout);
  //inline_imports(std::cout,std::cin);
  std::cout << std::flush;
  return 0;
}
