#ifndef PRED_FORMAT_H_
#define PRED_FORMAT_H_

#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/util/string.h"

namespace {

str show(Term t) {
  switch(t.type()) {
    case Term::VAR: return util::fmt("v%",Var(t).id());
    case Term::FUN: {
      Fun fun(t);
      if(fun.fun()==u64(Fun::EXTRA_CONST)) return "c";
      vec<str> args(fun.arg_count());
      for(size_t i=0; i<fun.arg_count(); ++i) args[i] = show(fun.arg(i));
      return util::fmt("f%(%)",Fun(t).fun(),util::join(",",args));
    }
  }
  error("unexpected t.type() = %",t.type());
}

str show(Atom a) {
  vec<str> args(a.arg_count());
  for(size_t i=a.arg_count(); i--;) args[i] = show(a.arg(i));
  str sign = a.sign() ? "+" : "-";
  str pred_name = a.pred()==Atom::EQ ? "eq" : util::fmt("p%",a.pred());
  return util::fmt("%%(%)",sign,pred_name,util::join(",",args));
}

str show(const OrClause &cla) {
  vec<str> atoms;
  for(size_t i=0; i<cla.atom_count(); ++i) atoms.push_back(show(cla.atom(i)));
  return util::join(" \\/ ",atoms);
}

str show(const AndClause &andClause) {
  vec<str> atoms;
  for(auto a : andClause.atoms) atoms.push_back(show(a));
  return util::join(" /\\ ",atoms);
}

str show(const NotAndForm &f) {
  vec<str> clauses;
  for(auto c : f.or_clauses) clauses.push_back(show(c) + "\n");
  return util::join("",clauses);
}

str show(const OrForm &f) {
  vec<str> clauses;
  for(auto c : f.and_clauses) clauses.push_back(show(c) + "\n");
  return util::join("",clauses);
}

}

#endif // PRED_FORMAT_H_

