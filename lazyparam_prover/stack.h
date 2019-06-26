#ifndef STACK_H_
#define STACK_H_

#include "lazyparam_prover/alloc.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/util/string.h"

using Branch = List<Atom>;

struct Bud {
  Branch branch;
};

inline str show(Branch b) {
  vec<str> atoms;
  for(; !b.empty(); b = b.tail()) atoms.push_back(show(b.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

inline str show(List<Bud> buds) {
  vec<str> branches;
  for(; !buds.empty(); buds = buds.tail()) branches.push_back(show(buds.head().branch)+"\n");
  return util::join("",branches);
}

// any clause with literal
// any subterm
// any weak strategy
// any branch term
// any strong strategy

#endif  // STACK_H_
