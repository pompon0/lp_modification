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
  Atom replace(memory::Alloc &x, Term a) const {
    return A.replace_arg(x,i,replace(x,A.arg(i),path,a));
  }
private:
  static Term replace(memory::Alloc &x, Term t, Path tp, Term a) {
    if(tp.empty()) return a;
    Fun tf(t);
    return Term(tf.replace_arg(x, tp.head(),replace(x, tf.arg(tp.head()),tp.tail(),a)));
  }
};

List<Path> paths(memory::Alloc &A, Term t) {
  if(t.type()==Term::VAR) return nothing();
  Fun f(t);
  List<Path> res;
  res.push(A,nothing());
  for(size_t i=f.arg_count(); i--;)
    for(auto l=paths(A,f.arg(i)); !l.empty(); l = l.tail())
      res.push(A,l.head().add(A,i));
  return res;
};

List<AtomPath> paths(memory::Alloc &A, Atom a) {
  List<AtomPath> res;
  for(size_t i=a.arg_count(); i--;)
    for(auto l=paths(A,a.arg(i)); !l.empty(); l = l.tail())
      res.push(A,AtomPath{a,i,l.head()});
  return res;
}

//////////////////////////////////////

struct ApClause : Lazy<DerAndClause>::Impl {
  ApClause(AtomPath _ap, Term _r) : ap(_ap), r(_r) {}
  AtomPath ap;
  Term r;

  DerAndClause get(memory::Alloc &A) const {
    auto A0 = ap.A;
    auto A1 = ap.replace(A,r);

    DerAndClause::Builder b(A);

    // build derived clause
    b.derived = AndClause::make(A,A0.neg(),A1,Atom::eq(A,true,ap.get(),r));

    // build atom monotonicity axiom.
    Term t0 = A0.arg(ap.i);
    Term t1 = A1.arg(ap.i);
    b.sources.push_back(AndClause::make(A,A0.neg(),A1,Atom::eq(A,true,t0,t1)));
    for(auto l=ap.path; !l.empty(); l = l.tail()) {
      Term s0 = Fun(t0).arg(l.head());
      Term s1 = Fun(t1).arg(l.head());
      b.sources.push_back(AndClause::make(A,Atom::eq(A,false,t0,t1),Atom::eq(A,true,s0,s1)));
      t0 = s0;
      t1 = s1;
    }
    return b.build();
  }
};

} // namespace lazy_paramodulation::tableau

#endif  // LAZY_PARAMODULATION_SPLIT_H_

