#ifndef PARSE2_H_
#define PARSE2_H_

#include "Parse/TPTP.hpp"
#include "lazyparam_prover/pred.h"

namespace tableau {

void parse() {
  auto units = Parse::TPTP::parse(std::cin);
}

} // namespace tableau

#endif  // PARSE2_H_
