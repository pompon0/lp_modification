#ifndef PROVER_OUTPUT_H
#define PROVER_OUTPUT_H

#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/kbo.h"

namespace tableau {

struct ProverOutput {
  size_t cont_count;
  size_t cost;
  KBO val;
  ptr<OrForm> proof;
};

} // namespace tableau

#endif  // PROVER_OUTPUT_H
