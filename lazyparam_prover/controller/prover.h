#ifndef CONTROLLER_PROVER_H_
#define CONTROLLER_PROVER_H_

#include <memory>
#include "lazyparam_prover/connection_tableau/cont.h"
#include "lazyparam_prover/controller/features.h"
#include "lazyparam_prover/controller/raw_prover.h"
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/lpmod.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/search_state.h"

namespace controller {

class Prover {  
  Prover() = default;

  struct Action {
    vec<size_t> raw_actions;
    features::ActionVec features; 
  };

  ptr<RawProver> p;
  vec<Action> actions;
  size_t features_space_size;

public:
  struct Save {
    using Ptr = std::shared_ptr<const Save>;
    RawProver::Save::Ptr p;
    vec<Action> actions;
  };

  Save::Ptr save() {
    PROF_CYCLES("Prover::save");
    return own(new Save{
      .p = p->save(),
      .actions = actions,
    });
  }
  void restore(Save::Ptr s) {
    PROF_CYCLES("Prover::restore");
    p->restore(s->p);
    actions = s->actions;
  }

  static ptr<Prover> New(Problem::Ptr problem, size_t features_space_size) {
    auto p = own(new Prover());
    p->p = RawProver::New(problem);
    p->features_space_size = features_space_size;
    //TODO: ugly, fix it.
    vec<size_t> i;
    p->find_actions(i);
    return p;
  }

  INL bool done() const { return p->done(); }

  INL size_t action_count() const { FRAME("action_count");
    DEBUG if(done()) error("already done");
    return actions.size(); 
  }

  INL features::StateVec state_features() const { return p->state_features(); }
  INL features::ActionVec action_features(size_t i) const {
    PROF_CYCLES("Prover::action_features");
    DEBUG if(i>=actions.size()) error("there are % actions",i,actions.size());
    return actions[i].features;
  }

  INL void apply_action(size_t i) { FRAME("apply_action(%)",i);
    PROF_CYCLES("Prover::apply_action");
    DEBUG if(done()) error("already done");
    DEBUG if(i>=actions.size()) error("i=%, but there are % actions",i,actions.size());
    for(auto ra : actions[i].raw_actions) p->apply_action(ra);
    actions.clear();
    //TODO: ugly, fix it.
    vec<size_t> ii;
    if(!done()) find_actions(ii);
  }

private:
  void find_actions(vec<size_t> &i) { FRAME("find_actions");
    auto s = p->save();
    auto ac = p->action_count();
    //TODO: perhaps replace i with a persistent list (requires additional alloc).
    i.push_back(0);
    for(; i.back()<ac; i.back()++) {
      if(p->action_is_mcts_node(i.back())) {
        actions.push_back({i,p->action_features(i.back(),features_space_size)});
      } else {
        p->apply_action(i.back());
        find_actions(i);
        p->restore(s);
      }
    }
    i.pop_back();
  }
};

} // namespace controller

#endif // CONTROLLER_PROVER_H_
