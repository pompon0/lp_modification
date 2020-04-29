#ifndef PROVER_OUTPUT_H
#define PROVER_OUTPUT_H

#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/kbo.h"
#include "solutions.pb.h"

namespace tableau {

struct Stats {
  size_t strong_only_steps = 0;

  solutions::ProverStats to_proto() const {
    solutions::ProverStats s;
    s.set_strong_only_steps(strong_only_steps);
    return s;
  }
};

Stats operator+(Stats a, Stats b) {
  return Stats {
    a.strong_only_steps + b.strong_only_steps,
  };
}
Stats & operator+=(Stats &a, Stats b){ return a = a+b; }

struct ProverOutput {
  size_t cont_count;
  size_t cost;
  KBO val;
  ptr<OrForm> proof;

  Stats stats;
};

} // namespace tableau

#endif  // PROVER_OUTPUT_H
