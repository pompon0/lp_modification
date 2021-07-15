#ifndef CONTROLLER_RAW_PROVER_H_
#define CONTROLLER_RAW_PROVER_H_

#include "lazyparam_prover/memory/list.h"
#include "lazyparam_prover/controller/problem.h"
#include "lazyparam_prover/connection_tableau/cont.h"

namespace controller {

struct Div {
  using Task = memory::function<void(Div*)>;
  struct GoalTask {
    memory::Maybe<tableau::Atom> goal;
    Task task;
  };
  using Cont = memory::List<GoalTask>; 
  struct Action { 
    // mcts_node is true iff continuation should be treated as a node in MCTS.
    // Otherwise, the head of the continuation should be executed immediately in search of MCTS nodes.
    // Current interpretation: mcts_node <=> just finished some unification.
    bool mcts_node = true;
    tableau::Branch branch;
    Cont cont;
    memory::List<tableau::Atom> new_goals; 
  };

  struct Alt {
    Div *div;
    Action action;
    template<typename F> INL void task(memory::Maybe<tableau::Atom> goal, F f){
      if(goal) {
        action.new_goals.push(div->A,goal.get());
      }
      action.cont.push(div->A,GoalTask{
        .goal = goal,
        .task = Task(div->A,f),
      });
    }

    INL void feature_branch(tableau::Branch b){ action.branch = b; }
    INL void feature_mcts_node(bool x){ action.mcts_node = x; }
  };

  INL size_t size_limit(){ return 1000000; }
  memory::Alloc &A;
  tableau::SearchState *state;
  template<typename F> INL void alt(F f){ FRAME("Div::alt");
    Alt alt{this,Action{.cont = _and}};
    f(&alt);
    next.push(A,alt.action); 
  }
  
  INL static memory::List<Action> Run(memory::Alloc &A, tableau::SearchState *state, Cont cont) { FRAME("Div::Run");
    DEBUG if(cont.empty()) error("empty cont");
    Div d{A,state,cont.tail()};
    cont.head().task(&d);
    return d.next;
  }

  Cont _and;
  memory::List<Action> next;
};

class RawProver {
  RawProver() = default;

  Problem::Ptr problem;
  ptr<memory::Alloc> A;
  ptr<tableau::SearchState> state;
  Div::Cont current;
  memory::List<Div::Action> next;

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
    Div::Cont current;
    memory::List<Div::Action> next;
    memory::Alloc::Save A;
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
      .current = current,
      .next = next,
      .A = A->save(),
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
    p->current = Div::Cont(*p->A,Div::GoalTask{
      .task = Div::Task(*p->A,[](Div *d){ tableau::connection_tableau::Cont::start(d); }),
    });
    p->next = Div::Run(*p->A,p->state.get(),p->current);
    return p;
  }

  INL bool done() const { return current.empty(); }

  INL size_t action_count() const { FRAME("action_count");
    DEBUG if(done()) error("already done");
    return next.size(); 
  }

  INL features::StateVec state_features(memory::Alloc &A, size_t hashed_size) const {
    PROF_CYCLES("RawProver::state_features");
    features::StateVec sv(A,hashed_size);
    sv.set_proof_size(state->nodes_used);
    sv.set_task_count(current.size());
    sv.set_total_vars_count(state->val.size());
    sv.set_free_vars_count(state->val.free_vars_size());
    size_t goals = 0;
    for(auto c = current; !c.empty(); c = c.tail()) {
      if(auto mg = c.head().goal; mg) {
        goals++;
        sv.add_goal(*(problem->node_idx.get()),state->val,mg.get());
      }
    }
    sv.set_goal_count(goals);
    return sv;
  }

  class ActionIter {
    size_t i;
    memory::List<Div::Action> next;
    RawProver *prover;
    ActionIter(size_t _i, memory::List<Div::Action> _next, RawProver *_prover) : i(_i), next(_next), prover(_prover) {}
    friend class RawProver;
  public:
    INL bool empty(){ return next.empty(); }
    INL ActionIter tail(){ return ActionIter(i+1,next.tail(),prover); }
    ActionIter at(size_t i){ return i ? tail().at(i-1) : *this; }
    
    INL size_t idx(){ return i; }
    INL bool is_mcts_node(){ return next.head().mcts_node; }
    INL features::ActionVec action_features(memory::Alloc &A, size_t hashed_size) const {
      PROF_CYCLES("RawProver::action_features");
      features::ActionVec av(A,hashed_size);
      auto a = next.head();
      av.set_new_goal_count(a.new_goals.size());
      av.set_path_length(a.branch.false_.size());
      for(auto p = a.branch.false_; !p.empty(); p = p.tail()) av.add_path(*(prover->problem->node_idx.get()),prover->state->val,p.head());
      for(auto g = a.new_goals; !g.empty(); g = g.tail()) av.add_new_goal(*(prover->problem->node_idx.get()),prover->state->val,g.head());
      return av;
    }

    INL void apply() { FRAME("apply_action(%)",i);
      PROF_CYCLES("RawProver::apply_action");
      DEBUG if(prover->done()) error("already done");
      //TODO: validate that action matches the prover instance.
      // And that action is still valid in the given state.
      prover->current = next.head().cont;
      if(!prover->current.empty()) prover->next = Div::Run(*prover->A,prover->state.get(),prover->current);
    }
  };

  INL ActionIter actions(){ return ActionIter(0,next,this); }
};

}  // namespace controller

#endif  // CONTROLLER_RAW_PROVER_H_
