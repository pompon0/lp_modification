#ifndef SYNTAX_SHOW_H_
#define SYNTAX_SHOW_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/util/string.h"

namespace tableau {

inline str show(Term t) {
  switch(t.type()) {
    case Term::VAR: return util::fmt("V%",Var(t).id());
    case Term::FUN: {
      Fun fun(t);
      switch(fun.fun()) {
        case Fun::EXTRA_CONST: return "c";
        case Fun::VAR_WRAP: return "v";
        case Fun::FUN_WRAP: return "f";
      }
      vec<str> args(fun.arg_count());
      for(size_t i=0; i<fun.arg_count(); ++i) args[i] = show(fun.arg(i));
      return util::fmt("f%(%)",Fun(t).fun(),util::join(",",args));
    }
  }
  error("unexpected t.type() = %",t.type());
}

inline str show_pred_name(u64 pred) {
  switch(pred) {
  case Atom::EQ: return "eq";
  case Atom::EQ_TRANS_POS: return "[eq]";
  case Atom::EQ_TRANS_NEG: return "{eq}";
  case Atom::EQ_SYMM: return "symm";
  }
  DEBUG if(s64(pred)<0) error("unnamed pred %",s64(pred));
  return util::fmt("p%",pred);
}

inline str show(Atom a) {
  vec<str> args(a.arg_count());
  for(size_t i=a.arg_count(); i--;) args[i] = show(a.arg(i));
  str sign = a.sign() ? "+" : "-";
  str pred_name = show_pred_name(a.pred());
  return util::fmt("%%(%)",sign,pred_name,util::join(",",args));
}

inline str show(const OrClause cla) {
  vec<str> atoms;
  for(size_t i=0; i<cla.atom_count(); ++i) atoms.push_back(show(cla.atom(i)));
  return util::join(" \\/ ",atoms);
}

inline str show(const AndClause cla) {
  vec<str> atoms;
  for(size_t i=0; i<cla.atom_count(); ++i) atoms.push_back(show(cla.atom(i)));
  return util::join(" /\\ ",atoms);
}

} // namespace tableau

#endif // SYNTAX_SHOW_H_

