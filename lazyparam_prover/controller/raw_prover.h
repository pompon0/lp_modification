#ifndef CONTROLLER_RAW_PROVER_H_
#define CONTROLLER_RAW_PROVER_H_

#include "lazyparam_prover/controller/problem.h"
#include "lazyparam_prover/connection_tableau/cont.h"

namespace controller {

struct Div {
  using Task = memory::function<void(Div*)>;
  using Cont = memory::List<Task>; 
  struct Action { 
    // mcts_node is true iff continuation should be treated as a node in MCTS.
    // Otherwise, the head of the continuation should be executed immediately in search of MCTS nodes.
    // Current interpretation: mctx_node <=> just finished some unification.
    bool mcts_node = true;
    tableau::Branch branch;
    vec<tableau::Atom> goals;
    Cont cont;
  };

  struct Alt {
    Div *div;
    Action action;
    template<typename F> INL void task(F f){ action.cont.push(div->A,Task(div->A,f)); }

    INL void feature_branch(tableau::Branch b){ action.branch = b; }
    INL void feature_mcts_node(bool x){ action.mcts_node = x; }
    INL void feature_goal(tableau::Atom goal){ action.goals.push_back(goal); }
  };

  INL size_t size_limit(){ return 1000000; }
  memory::Alloc &A;
  tableau::SearchState *state;
  template<typename F> INL void alt(F f){ FRAME("Div::alt");
    Alt alt{this,Action{.cont = _and}};
    f(&alt);
    next.push_back(alt.action); 
  }
  
  INL static vec<Action> Run(memory::Alloc &A, tableau::SearchState *state, Cont cont) { FRAME("Div::Run");
    DEBUG if(cont.empty()) error("empty cont");
    Div d{A,state,cont.tail()};
    cont.head()(&d);
    return d.next;
  }

  Cont _and;
  vec<Action> next;
};

class RawProver {
  RawProver() = default;

  Problem::Ptr problem;
  ptr<memory::Alloc> A;
  ptr<tableau::SearchState> state;
  Div::Cont current; // (state + current) define state features
  vec<Div::Action> next; // (next[i]-current) define action features
 
  DEBUG_ONLY(
    vec<size_t> saves;
    size_t serial_counter = 0;
  )

public:
  struct Save {
    using Ptr = std::shared_ptr<const Save>; 
    DEBUG_ONLY(
      RawProver *prover;
      size_t serial_id;
      size_t id;
    )
    tableau::SearchState::Save state;
    memory::Alloc::Save A;
    Div::Cont current;
    vec<Div::Action> next;
  };
  
  Save::Ptr save() {
    PROF_CYCLES("RawProver::save");
    DEBUG_ONLY({
      saves.push_back(serial_counter++);
    })
    return own(new Save{
      DEBUG_ONLY(
        .prover = this,
        .serial_id = saves.back(),
        .id = saves.size()-1,
      )
      .state = state->save(),
      .A = A->save(),
      .current = current,
      .next = next,
    });
  }

  void restore(Save::Ptr s) {
    PROF_CYCLES("RawProver::restore");
    DEBUG_ONLY({
      if(s->prover!=this) error("Save of a different prover");
      if(s->id>=saves.size()) error("s.id = %, want < %",s->id,saves.size());
      if(s->serial_id!=saves[s->id]) error("s.serial_id = %, want %",s->serial_id,saves[s->id]);
      saves.erase(saves.begin()+s->id+1,saves.end());
    })
    A->restore(s->A);
    state->restore(s->state);
    current = s->current;
    next = s->next;
  }

  static ptr<RawProver> New(Problem::Ptr problem) {
    auto p = own(new RawProver());
    p->problem = problem;
    p->A = make<memory::Alloc>();

    p->state = make<tableau::SearchState>(*problem->idx,FunOrd());
    p->current = Div::Cont(*p->A,Div::Task(*p->A,[](Div *d){ tableau::connection_tableau::Cont::start(d); }));
    p->next = Div::Run(*p->A,p->state.get(),p->current);
    return p;
  }

  INL bool done() const { return current.empty(); }

  INL size_t action_count() const { FRAME("action_count");
    DEBUG if(done()) error("already done");
    return next.size(); 
  }

  INL features::StateVec state_features() const {
    PROF_CYCLES("RawProver::state_features");
    return {
      .proof_size = state->nodes_used,
      .open_branches = current.size(),
    };
  }

  INL bool action_is_mcts_node(size_t i) { FRAME("action_is_mcts_node");
    DEBUG if(i>=next.size()) error("i=%, but there are % actions",i,next.size());
    return next[i].mcts_node;
  }

  INL features::ActionVec action_features(size_t i, size_t hashed_size) const {
    PROF_CYCLES("RawProver::action_features");
    DEBUG if(i>=next.size()) error("i=%, but there are % actions",i,next.size());
    features::ActionVec av(hashed_size);
    av.set_goal_count(next[i].goals.size());
    av.set_path_length(next[i].branch.false_.size());
    for(auto p = next[i].branch.false_; !p.empty(); p = p.tail()) av.add_path(*(problem->node_idx.get()),state->val,p.head());
    for(auto g : next[i].goals) av.add_goal(*(problem->node_idx.get()),state->val,g);
    return av;
  }

  INL void apply_action(size_t i) { FRAME("apply_action(%)",i);
    PROF_CYCLES("RawProver::apply_action");
    DEBUG if(done()) error("already done");
    DEBUG if(i>=next.size()) error("i=%, but there are % actions",i,next.size());
    current = next[i].cont;
    if(!current.empty()) next = Div::Run(*A,state.get(),current);
  }
};

}  // namespace controller

#endif  // CONTROLLER_RAW_PROVER_H_
