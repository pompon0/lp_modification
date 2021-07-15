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
#include "lazyparam_prover/memory/stack.h"

namespace controller {

class Prover {  
  Prover() = default;

  struct Action {
    memory::List<size_t> raw_actions;
    features::ActionVec features; 
  };

  ptr<RawProver> p;
  //TODO: consider creating a smaller block size.
  ptr<memory::Alloc> actions_alloc;
  memory::List<Action> actions;

  INL Action actions_at(size_t i) const {
    auto a = actions;
    while(i--) a = a.tail();
    return a.head();
  }

  size_t features_space_size;

public:
  struct Save {
    using Ptr = std::shared_ptr<const Save>;
    RawProver::Save::Ptr p;
    memory::Alloc::Save actions_alloc;
    memory::List<Action> actions;
  };

  Save::Ptr save() {
    PROF_CYCLES("Prover::save");
    return own(new Save{
      .p = p->save(),
      .actions_alloc = actions_alloc->save(),
      .actions = actions,
    });
  }
  void restore(Save::Ptr s) {
    PROF_CYCLES("Prover::restore");
    p->restore(s->p);
    actions_alloc->restore(s->actions_alloc);
    actions = s->actions;
  }

  static ptr<Prover> New(Problem::Ptr problem, size_t features_space_size) {
    auto p = own(new Prover());
    p->p = RawProver::New(problem);
    p->features_space_size = features_space_size;
    p->actions_alloc = make<memory::Alloc>();
    p->find_actions(memory::nothing());
    return p;
  }

  INL bool done() const { return p->done(); }

  INL size_t action_count() const { FRAME("action_count");
    DEBUG if(done()) error("already done");
    return actions.size(); 
  }

  INL features::StateVec state_features() const { return p->state_features(*actions_alloc,features_space_size); }
  INL features::ActionVec action_features(size_t i) const {
    PROF_CYCLES("Prover::action_features");
    DEBUG if(i>=actions.size()) error("there are % actions",i,actions.size());
    return actions_at(i).features;
  }

  INL void apply_action(size_t i) { FRAME("apply_action(%)",i);
    PROF_CYCLES("Prover::apply_action");
    DEBUG if(done()) error("already done");
    DEBUG if(i>=actions.size()) error("i=%, but there are % actions",i,actions.size());
    apply_raw_actions(actions_at(i).raw_actions);
    // reset actions storage before listing next actions.
    actions = memory::nothing();
    if(!done()) {
      PROF_CYCLES("Prover::find_actions (in Prover::apply_action)");
      find_actions(memory::nothing());
    }
  }

private:
  void apply_raw_actions(memory::List<size_t> path) {
    //TODO: you might need to force recursion unfolding.
    if(path.empty()) return;
    apply_raw_actions(path.tail());
    p->actions().at(path.head()).apply();
  }

  void find_actions(memory::List<size_t> pref) { FRAME("find_actions");
    RawProver::Save::Ptr s;
    size_t ac;
    {
      PROF_CYCLES("Prover::find_actions (save & action_count)");
      s = p->save();
      ac = p->action_count();
    }
    for(auto it = p->actions(); !it.empty(); it = it.tail()) {
      if(it.is_mcts_node()) {
        actions.push(*actions_alloc,{
          pref.add(*actions_alloc,it.idx()),
          it.action_features(*actions_alloc,features_space_size),
        });
      } else {
        it.apply();
        find_actions(pref.add(*actions_alloc,it.idx()));
        p->restore(s);
      }
    }
  }
};

} // namespace controller

#endif // CONTROLLER_PROVER_H_
