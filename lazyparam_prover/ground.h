#ifndef GROUND_H_
#define GROUND_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"

namespace tableau {

// clears offset (it doesnt' matter, because result contains no vars)
inline Term ground(memory::Alloc &A, Term t) { FRAME("ground(%)",show(t));
  switch(t.type()) {
    case Term::VAR: return Term(Fun::Builder(A,Fun::EXTRA_CONST,0).build());
    case Term::FUN: {
      Fun tf(t);
      size_t ac = tf.arg_count();
      Fun::Builder b(A,tf.fun(),ac);
      for(size_t i=0; i<ac; ++i) b.set_arg(i,ground(A,tf.arg(i)));
      return Term(b.build());
    }
    default: error("unhandled t.type() = %",t.type());
  }
}

// clears offset
inline Atom ground(memory::Alloc &A, Atom a) { FRAME("ground(%)",show(a));
  size_t ac = a.arg_count();
  Atom::Builder b(A,a.sign(),a.pred(),ac,a.strong_only());
  for(size_t i=ac; i--;) b.set_arg(i,ground(A,a.arg(i)));
  return b.build();
}

// clears offset
inline AndClause ground(memory::Alloc &A, AndClause cla) { FRAME("ground(%)",show(cla));
  AndClause::Builder b(A,cla.atom_count());
  for(size_t i=cla.atom_count(); i--;) b.set_atom(i,ground(A,cla.atom(i)));
  return b.build();
}

}  // namespace tableau

#endif  // GROUND_H_
