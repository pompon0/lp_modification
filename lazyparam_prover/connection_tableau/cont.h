#ifndef CONNECTION_TABLEAU_CONT_H_
#define CONNECTION_TABLEAU_CONT_H_

#include "utils/ctx.h"
#include "utils/log.h"
#include "utils/types.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/function.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/constrained_valuation.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/prover_output.h"

namespace tableau::connection_tableau {

struct Cont {
  #include "lazyparam_prover/connection_tableau/frames/start.h"
  #include "lazyparam_prover/connection_tableau/frames/strong.h"
  #include "lazyparam_prover/connection_tableau/frames/weak.h"
  template<typename Div> INL static void start(Div *d){ _StartFrame{}.run(d); }
};

}  // namespace tableau::connection_tableau

#endif  // CONNECTION_TABLEAU_CONT_H_
