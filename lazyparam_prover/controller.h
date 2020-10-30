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
  friend class Prover;
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
  using Task = memory::function<void(Div*)>;
  using Cont = memory::List<Task>;

  enum { size_limit = 1000000 };
  memory::Alloc &A;
  tableau::SearchState *state;
  template<typename F> INL void or_(tableau::Features x, F f){ next.push_back(_and.add(A,Task(A,f))); }
  template<typename F> INL void and_(F f){ _and.push(A,Task(A,f)); }
  INL void done(tableau::Features f){ next.push_back(_and); }
  
  INL static vec<Cont> Run(memory::Alloc &A, tableau::SearchState *state, Cont cont) { FRAME("Div::Run");
    DEBUG if(cont.empty()) error("empty cont");
    Div d{A,state,cont.tail()};
    cont.head()(&d);
    return d.next;
  }
  Cont _and;
  vec<Cont> next;
};

// If we decide items order before MCTS, it has to be based on the problem features.
//
// We can however have a separate model to evaluate each new branch separately
// and sort the items according to that evaluation. But that would have to be
// trained on resulting search space size.

// should we flatten the search tree for MCTS?
// shouldn't this be rather a state diff? IMO it should summarize all new branches
struct ActionFeaturesVec {
  size_t new_branches;
  // number of new pos/neg equality branches
  // (?) number of new branches per task type
};

struct StateFeaturesVec {
  size_t proof_size; // in clauses
  size_t open_branches;
  // proof size in literals
  // depth of the current branch
  // depth of the open branches
  // total proof depth
  // number of variables 
  // number of free variables (total/in current branch)
  // number of free variables present only in a single branch
  // number of literals per predicate? (can we do that?)
};

class Prover {  
  Prover() = default;

  ptr<memory::Alloc> A;
  ptr<tableau::SearchState> state;
  Div::Cont current; // (state + current) define state features
  vec<Div::Cont> next; // (next[i]-current) define action features
 
  struct Save {
    DEBUG_ONLY(size_t serial_id;)
    tableau::SearchState::Save state;
    memory::Alloc::Save A;
    Div::Cont current;
    vec<Div::Cont> next;
  };
  vec<Save> saves;
  DEBUG_ONLY(size_t serial_counter = 0;)
public:
  struct SaveId {
    DEBUG_ONLY(
      Prover *prover;
      size_t serial_id;
    )
    size_t id;
  };

  SaveId save() {
    saves.push_back({
      DEBUG_ONLY(.serial_id = serial_counter++,)
      .state = state->save(),
      .A = A->save(),
      .current = current,
      .next = next,
    });
    return {
      DEBUG_ONLY(
        .prover = this,
        .serial_id = saves.back().serial_id,
      )
      .id = saves.size()-1,
    };
  }

  void restore(SaveId s) {
    DEBUG_ONLY({
      if(s.prover!=this) error("Save of a different prover");
      if(s.id>=saves.size()) error("s.id = %, want < %",s.id,saves.size());
      if(saves[s.id].serial_id!=s.serial_id) error("s.serial_id = %, want %",s.id,saves[s.id].serial_id);
    })
    A->restore(saves[s.id].A);
    state->restore(saves[s.id].state);
    current = saves[s.id].current;
    next = saves[s.id].next;
    saves.erase(saves.begin()+s.id+1,saves.end());
  }

  static ptr<Prover> New(Problem::Ptr problem) {
    auto p = own(new Prover());
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
      .new_branches = next[i].size()-current.size(),
    };
  }

  INL void apply_action(size_t i) { FRAME("apply_action(%)",i);
    DEBUG if(done()) error("already done");
    DEBUG if(i>=next.size()) error("there are % actions",i,next.size());
    current = next[i];
    if(!current.empty()) next = Div::Run(*A,state.get(),current);
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
