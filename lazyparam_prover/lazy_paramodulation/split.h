#ifndef LAZY_PARAMODULATION_SPLIT_H_
#define LAZY_PARAMODULATION_SPLIT_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/memory/maybe.h"

namespace tableau::lazy_paramodulation {

struct Tp { Term T; Fun p; };
struct Ap { Atom A; Fun p; };

List<Tp> split(Term T, Var w) {
  if(T.type()!=Term::FUN) return nothing();
  Fun f(T);
  List<Tp> res(Tp{Term(w),f});
  for(size_t i=0; i<f.arg_count(); i++)
    for(auto l=split(f.arg(i),w); !l.empty(); l = l.tail())
      res += Tp{Term(f.replace_arg(i,l.head().T)),l.head().p};
  return res;
}

List<Ap> split(Atom A, Var w) {
  List<Ap> res;
  for(size_t i=0; i<A.arg_count(); i++)
    for(auto l=split(A.arg(i),w); !l.empty(); l = l.tail())
      res += Ap{A.replace_arg(i,l.head().T),l.head().p};
  return res;
}

} // namespace lazy_paramodulation::tableau

#endif  // LAZY_PARAMODULATION_SPLIT_H_

