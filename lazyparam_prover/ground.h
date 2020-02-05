#ifndef GROUND_H_
#define GROUND_H_

#include "lazyparam_prover/pred.h"

namespace tableau {

// clears offset (it doesnt' matter, because result contains no vars)
inline Term ground(Term t) { FRAME("ground(%)",show(t));
  switch(t.type()) {
    case Term::VAR: return Term(Fun::Builder(Fun::EXTRA_CONST,0).build());
    case Term::FUN: {
      Fun tf(t);
      size_t ac = tf.arg_count();
      Fun::Builder b(tf.fun(),ac);
      for(size_t i=0; i<ac; ++i) b.set_arg(i,ground(tf.arg(i)));
      return Term(b.build());
    }
    default: error("unhandled t.type() = %",t.type());
  }
}

// clears offset
inline Atom ground(Atom a) { FRAME("ground(%)",show(a));
  size_t ac = a.arg_count();
  Atom::Builder b(a.sign(),a.pred(),ac);
  for(size_t i=ac; i--;) b.set_arg(i,ground(a.arg(i)));
  return b.build();
}

// clears offset
inline OrClause ground(OrClause cla) { FRAME("ground(%)",show(cla));
  OrClause::Builder b(cla.atom_count(),0);
  for(size_t i=cla.atom_count(); i--;) b.set_atom(i,ground(cla.atom(i)));
  return b.build();
}

}  // namespace tableau

#endif  // GROUND_H_
