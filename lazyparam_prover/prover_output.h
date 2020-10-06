#ifndef PROVER_OUTPUT_H
#define PROVER_OUTPUT_H

#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/kbo.h"
#include "solutions.pb.h"

namespace tableau {

struct Stats {
  size_t steps = 0;
  size_t strong_steps = 0;
  size_t strong_only_steps = 0;
  size_t weak_connections_steps = 0;
  size_t weak_set_steps = 0;
  size_t weak_steps = 0;
  size_t weak_unify_steps = 0;
  size_t min_cost_steps = 0;
  ValuationStats val;
  
  solutions::ProverStats to_proto() const {
    solutions::ProverStats s;
    s.set_strong_steps(strong_steps);
    s.set_strong_only_steps(strong_only_steps);
    s.set_weak_connections_steps(weak_connections_steps);
    s.set_weak_set_steps(weak_set_steps);
    s.set_weak_steps(weak_steps);
    s.set_min_cost_steps(min_cost_steps);
    s.set_val_unifications(val.unifications);
    s.set_val_failed_unifications(val.failed_unifications);
    s.set_val_broken_constraints(val.broken_constraints);
    s.set_val_comparisons(val.comparisons);
    return s;
  }
};

Stats & operator+=(Stats &a, Stats b){
  a.strong_steps += b.strong_steps;
  a.strong_only_steps += b.strong_only_steps;
  a.weak_connections_steps += b.weak_connections_steps;
  a.weak_set_steps += b.weak_set_steps;
  a.weak_steps += b.weak_steps;
  a.weak_unify_steps += b.weak_unify_steps;
  a.min_cost_steps += b.min_cost_steps;
  a.val += b.val;
  return a;
}

Stats operator+(Stats a, Stats b) { return a += b; }

struct ProverOutput {
  size_t cont_count;
  size_t cost;
  Valuation val;
  ptr<OrForm> proof;

  Stats stats;

  INL ProverOutput() = default;
  INL ProverOutput(ProverOutput&&) = default;
  INL ProverOutput& operator=(ProverOutput&&) = default;
  ~ProverOutput(){}
};

} // namespace tableau

#endif  // PROVER_OUTPUT_H
