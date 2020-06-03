#ifndef LAZY_PARAMODULATION_SPLIT_H_
#define LAZY_PARAMODULATION_SPLIT_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/memory/lazy.h"
#include "lazyparam_prover/memory/maybe.h"
#include "lazyparam_prover/derived.h"

namespace tableau::lazy_paramodulation {

using Path = List<size_t>;
struct AtomPath {
  Atom A;
  size_t i; Path path;
  Term get() const {
    Term t = A.arg(i);
    for(auto p=path; !p.empty(); p=p.tail()) t = Fun(t).arg(p.head());
    return t;
  }
  Atom replace(Term a) const {
    return A.replace_arg(i,replace(A.arg(i),path,a));
  }
private:
  static Term replace(Term t, Path tp, Term a) {
    if(tp.empty()) return a;
    Fun tf(t);
    return Term(tf.replace_arg(tp.head(),replace(tf.arg(tp.head()),tp.tail(),a)));
  }
};

List<Path> paths(Term t) {
  if(t.type()==Term::VAR) return nothing();
  Fun f(t);
  List<Path> res;
  res += nothing();
  for(size_t i=f.arg_count(); i--;)
    for(auto l=paths(f.arg(i)); !l.empty(); l = l.tail())
      res += i+l.head();
  return res;
};

List<AtomPath> paths(Atom a) {
  List<AtomPath> res;
  for(size_t i=a.arg_count(); i--;)
    for(auto l=paths(a.arg(i)); !l.empty(); l = l.tail())
      res += AtomPath{a,i,l.head()};
  return res;
}

//////////////////////////////////////

struct ApClause : Lazy<DerAndClause>::Impl {
  ApClause(AtomPath _ap, Term _r) : ap(_ap), r(_r) {}
  AtomPath ap;
  Term r;

  DerAndClause get() const {
    auto A0 = ap.A;
    auto A1 = ap.replace(r);

    DerAndClause::Builder b;

    // build derived clause
    b.derived = AndClause::make(A0.neg(),A1,Atom::eq(true,ap.get(),r));

    // build atom monotonicity axiom.
    Term t0 = A0.arg(ap.i);
    Term t1 = A1.arg(ap.i);
    b.sources.push_back(AndClause::make(A0.neg(),A1,Atom::eq(true,t0,t1)));
    for(auto l=ap.path; !l.empty(); l = l.tail()) {
      Term s0 = Fun(t0).arg(l.head());
      Term s1 = Fun(t1).arg(l.head());
      b.sources.push_back(AndClause::make(Atom::eq(false,t0,t1),Atom::eq(true,s0,s1)));
      t0 = s0;
      t1 = s1;
    }
    return b.build();
  }
};

} // namespace lazy_paramodulation::tableau

#endif  // LAZY_PARAMODULATION_SPLIT_H_
