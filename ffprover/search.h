#ifndef FFPROVER_SEARCH_H_
#define FFPROVER_SEARCH_H_

#include <functional>
#include "lazyparam_prover/controller.h"
#include "ffprover/tree.h"

namespace ff {

struct Search {
  struct Config { 
    bool one_expansion_per_playout;
    size_t playouts_per_bigstep;
    size_t playout_depth;
    std::function<double(controller::StateFeaturesVec)> base_reward; // [0,1]
    std::function<double(controller::ActionFeaturesVec)> base_priority; 
    std::function<double(Tree::Ptr,size_t)> child_priority;
    std::function<size_t(Tree::Ptr)> bigstep_selector;
  };

  struct Stats {
    size_t inferences = 0;
    size_t bigsteps = 0;
  };

  Search(Config _cfg) : cfg(_cfg) {}

  Config cfg;
  Stats stats;

  enum Result { SOLVED, DEADEND, CANCELLED };

  Result run(Ctx::Ptr ctx, Tree::Ptr t, controller::Prover &p) {
    while(!ctx->done()) {
      auto s = p.save();
      for(size_t i = 0; i<cfg.playouts_per_bigstep; i++) {
        p.restore(s);
        playout(t,p);
      }
      if(p.done()) return SOLVED;
      if(t.lost()) return DEADEND;
      auto i = cfg.bigstep_selector(t);
      stats.bigsteps++;
      t = t.child(i);
      p.apply_action(i);
    }
    return CANCELLED;
  }

private:
  double reward(Tree::Ptr t, controller::Prover &p) {
    if(p.done()) return 1.;
    if(t.lost()) return 0.;
    auto r = cfg.base_reward(p.state_features());
    DEBUG if(r>1. || r<0.) error("cfg.base_reward() = %, want in [0,1]",r);
    return r;
  }

  void expand(Tree::Ptr t, controller::Prover &p) {
    size_t n = p.action_count();
    vec<double> priorities(n);
    for(size_t i=0; i<n; i++) priorities[i] = cfg.base_priority(p.action_features(i));
    t.expand(priorities);
  }

  size_t select_child(Tree::Ptr t) {
    if(t.child_count()==0) error("t has no children");
    size_t best_i=0; double best = 0;
    for(size_t i=0; i<t.child_count(); i++) {
      auto x = cfg.child_priority(t,i);
      if(x>best){ best_i = i; best = x; }
    }
    return best_i;
  }

  void playout(Tree::Ptr t, controller::Prover &p) {
    size_t expansions = 0;
    for(size_t d = cfg.playout_depth; d--;) {
      if(p.done()) break;
      if(!t.expanded()) {
        if(expansions && cfg.one_expansion_per_playout) break;
        expansions++;
        expand(t,p);
        if(t.lost()) break;
      }
      DEBUG if(t.lost()) error("playout in a lost branch");
      auto i = select_child(t);
      t = t.child(i);
      p.apply_action(i);
      stats.inferences++;
    }
    t.visit(reward(t,p));
  } 
};

}  // namespace ff

#endif  // FFPROVER_SEARCH_H_
