#ifndef CONTROLLER_H_
#define CONTROLLER_H_

#include <memory>
#include "lazyparam_prover/index.h"
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/lpmod.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/connection_tableau/cont.h"
#include "tool/bin/wrapper.h"

// TODO: apart running ML to choose the right alternative to find the answer,
//   run ML to minimize the search space by an appropriate choice of task order
namespace controller {

class Problem {
public:
  using Ptr = std::shared_ptr<const Problem>;
  static Ptr New(const str &tptp_fof) {
    auto file = tptp_to_proto(tptp_cnf(tptp_fof));
    auto p = own(new Problem());
    p->A = make<memory::Alloc>();
    auto f = tableau::ParseCtx().parse_orForm(*p->A,file);
    p->idx = make<tableau::ClauseIndex>(tableau::lpmod::conv(*p->A,f));
    return p;
  }
  friend class RawProver;
private:
  Problem() = default;
  ptr<memory::Alloc> A;
  ptr<tableau::ClauseIndex> idx;

  static str tptp_cnf(const str &tptp_fof) {
    tool::Request req;
    req.mutable_fof_to_cnf()->set_tptp_fof(tptp_fof);
    return tool::bin::wrapper(Ctx::background(),req).fof_to_cnf().tptp_cnf();
  }
  static tptp::File tptp_to_proto(const str &tptp_cnf) {
    tool::Request req;
    req.mutable_tptp_to_proto()->set_tptp(tptp_cnf);
    req.mutable_tptp_to_proto()->set_lang(tptp::Input::CNF);
    return tool::bin::wrapper(Ctx::background(),req).tptp_to_proto().file();
  }
};

struct Div {
  struct Features : tableau::Features {
    void set_new_goals(memory::List<tableau::Atom> goals) {
      for(; goals; goals = goals.tail()) {
        auto a = goals.head();
      }
    }
  };

  using Task = memory::function<void(Div*)>;
  using Cont = memory::List<Task>; 
  struct Action {
    tableau::Features features;
    Cont cont;
  };

  enum { size_limit = 1000000 };
  memory::Alloc &A;
  tableau::SearchState *state;
  template<typename F> INL void or_(tableau::Features x, F f){ next.push_back(Action{x,_and.add(A,Task(A,f))}); }
  template<typename F> INL void and_(F f){ _and.push(A,Task(A,f)); }
  INL void done(tableau::Features f){ next.push_back(Action{f,_and}); }
  
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

  INL StateFeaturesVec state_features() const {
    return {
      .proof_size = state->nodes_used,
      .open_branches = current.size(),
    };
  }
  INL ActionFeaturesVec action_features(size_t i) const {
    DEBUG if(i>=next.size()) error("there are % actions",i,next.size());
    return {
      .mcts_node = next[i].features.mcts_node,
      .new_branches = next[i].cont.size()-current.size(),
    };
  }

  INL void apply_action(size_t i) { FRAME("apply_action(%)",i);
    DEBUG if(done()) error("already done");
    DEBUG if(i>=next.size()) error("i=%, but there are % actions",i,next.size());
    current = next[i].cont;
    if(!current.empty()) next = Div::Run(*A,state.get(),current);
  }
};

class Prover {  
  Prover() = default;

  struct Action {
    vec<size_t> raw_actions;
    ActionFeaturesVec features; 
  };

  ptr<RawProver> p;
  vec<Action> actions;

public:
  struct Save {
    using Ptr = std::shared_ptr<const Save>;
    RawProver::Save::Ptr p;
    vec<Action> actions;
  };

  Save::Ptr save() {
    return own(new Save{
      .p = p->save(),
      .actions = actions,
    });
  }
  void restore(Save::Ptr s) {
    p->restore(s->p);
    actions = s->actions;
  }

  static ptr<Prover> New(Problem::Ptr problem) {
    auto p = own(new Prover());
    p->p = RawProver::New(problem);
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

  INL StateFeaturesVec state_features() const { return p->state_features(); }
  INL ActionFeaturesVec action_features(size_t i) const {
    DEBUG if(i>=actions.size()) error("there are % actions",i,actions.size());
    return actions[i].features;
  }

  INL void apply_action(size_t i) { FRAME("apply_action(%)",i);
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
      if(auto af = p->action_features(i.back()); af.mcts_node) {
        actions.push_back({i,af});
      } else {
        p->apply_action(i.back());
        find_actions(i);
        p->restore(s);
      }
    }
    i.pop_back();
  }
};
/*bool search(const Ctx &ctx, Prover &p) { FRAME("controller::search");
  SCOPE("controller::search");
  vec<Prover::Action> as{p.reset()};
  size_t steps = 0;
  for(;as.size(); steps++) {
    Prover::Action a = as.back();
    as.pop_back();
    if(a.done()) return true;
    if(steps%100==0 && ctx.done()) break;
    DEBUG if(steps%1000==0) info("steps = %",steps);
    for(auto x : p.run(a)) as.push_back(x);
  }
  DEBUG info("steps = %",steps);
  return false;
}*/

} // namespace controller

#endif // CONTROLLER_H_
