#ifndef FFPROVER_SEARCH_H_
#define FFPROVER_SEARCH_H_

#include <functional>
#include "lazyparam_prover/controller/features.h"
#include "lazyparam_prover/controller/prover.h"
#include "ffprover/tree.h"
#include "ffprover/xgboost.h"
#include "utils/log.h"

namespace ff {

struct Result {
  enum Status { SOLVED, DEADEND, CANCELLED };
  Status status;
  Tree::Ptr node;

  struct Stats {
    size_t inferences = 0;
    size_t playouts = 0;
    size_t bigsteps = 0;
  };
  Stats stats;
};

struct Search {
  struct Config { 
    bool one_expansion_per_playout;
    size_t playouts_per_bigstep;
    size_t playout_depth;
    std::function<double(features::StateVec)> base_reward; // [0,1]
    std::function<double(features::ActionVec)> base_priority; 
    std::function<double(Tree::Ptr,size_t)> child_priority;
    std::function<size_t(Tree::Ptr)> bigstep_selector;
  };

  Search(Config _cfg) : cfg(_cfg) {
    if(auto got = cfg.playouts_per_bigstep, want = 0ul; got<want) {
      error("cfg.playouts_per_bigstep = %, want >=%",got,want);
    }
    if(auto got = cfg.playout_depth, want = 0ul; got<=want) {
      error("cfg.playout_depth = %, want >%",got,want);
    }
  }

  Config cfg;
  Result::Stats stats;

  Result run(Ctx::Ptr ctx, Tree::Ptr t, controller::Prover &p) { FRAME("Search::run");
    PROF_TIME("Search::run");
    while(!ctx->done()) {
      info("bigsteps = %",stats.bigsteps);
      // early exit
      if(p.done()) return {.status = Result::SOLVED, .node = t, .stats = stats};
      if(t.lost()) return {.status = Result::DEADEND, .node = t, .stats = stats};
      if(!t.expanded()) expand(t,p);
      // perform playouts
      auto s = p.save();
      if(t.child_count()>1) {
        for(size_t i = 0; !ctx->done() && i<cfg.playouts_per_bigstep; i++) {
          playout(t,p);
          p.restore(s);
        }
      }
      if(ctx->done()) break;
      auto i = cfg.bigstep_selector(t);
      info("bigstep_selector() = %/%",i,t.child_count());
      stats.bigsteps++;
      t = t.child(i);
      p.apply_action(i);
    }
    return {.status = Result::CANCELLED, .node = t, .stats = stats};
  }
  
private:
  double reward(Tree::Ptr t, controller::Prover &p) {
    if(p.done()) return 1.;
    if(t.lost()) return 0.;
    auto r = cfg.base_reward(p.state_features());
    DEBUG if(r>1. || r<0.) error("cfg.base_reward() = %, want in [0,1]",r);
    return r;
  }

  void expand(Tree::Ptr t, controller::Prover &p) { FRAME("search::expand()");
    size_t n = p.action_count();
    vec<double> priorities(n);
    for(size_t i=0; i<n; i++) priorities[i] = cfg.base_priority(p.action_features(i));
    t.expand(priorities);
  }

  size_t select_child(Tree::Ptr t) {
    DEBUG if(t.child_count()==0) error("t has no children");
    ssize_t best_i=-1; double best = -1;
    for(size_t i=0; i<t.child_count(); i++) {
      if(t.child(i).lost()) continue;
      auto x = cfg.child_priority(t,i);
      if(best_i==-1 || x>best){ best_i = i; best = x; }
    }
    if(best_i==-1) error("all children lost");
    return size_t(best_i);
  }

  void playout(Tree::Ptr t, controller::Prover &p) {
    PROF_TIME("Search::playout");
    stats.playouts++;
    size_t expansions = 0;
    //str path = "";
    for(size_t d = cfg.playout_depth; d--;) {
      if(t.lost()) return;
      if(p.done()) break;
      if(!t.expanded()) {
        if(expansions && cfg.one_expansion_per_playout) break;
        expand(t,p);
        expansions++;
        if(t.lost()) break;
      }
      DEBUG if(t.lost()) error("playout in a lost branch");
      auto i = select_child(t);
      //path += util::fmt("%/% ",i,t.child_count());
      t = t.child(i);
      p.apply_action(i);
      stats.inferences++;
    }
    t.visit(reward(t,p));
    if(p.done()) t.set_won();
  }
};

INL static inline double logit(double x) {
  if(x==1.) return 10.;
  if(x==0.) return -10;
  auto ret = log(x/(1.-x));
  if(ret<-10.) return -10.;
  if(ret>10.) return 10.;
  return ret;
}

INL static inline FeatureVec action_features(features::ActionVec a) {
  FeatureVec v(a.features.v.size());
  for(size_t i = 0; i<a.features.v.size(); i++) v.push(i,a.features.v[i]);
  return v;
}
INL static inline FeatureVec state_features(features::StateVec a) {
  FeatureVec v(a.features.v.size());
  for(size_t i = 0; i<a.features.v.size(); i++) v.push(i,a.features.v[i]);
  return v;
}

static void collect_output(mcts::Path *path, Tree::Ptr t, controller::Prover &prover, double reward) {
  if(!t.has_parent()) return;
  collect_output(path,t.parent(),prover,reward*0.98); 
  auto *node = path->add_nodes();
  size_t i = t.parent().child_id(t);
  node->set_chosen_action(i);
  auto norm_vsum = double(t.parent().visits())/double(t.parent().child_count());
  for(size_t i=0; i<t.parent().child_count(); i++) {
    double v = t.parent().child(i).visits();
    auto *action = node->add_actions();
    // this is dual to the exp in the base_priority
    action->set_label(std::max<double>(-6,log(v/norm_vsum)));
    action_features(prover.action_features(i)).to_proto(*action);
  }
  auto *state = node->mutable_state();
  state->set_label(logit(reward)); // TODO: should it really be [-10,10] ?
  state_features(prover.state_features()).to_proto(*state);
  prover.apply_action(i);
  return;
}

}  // namespace ff

#endif  // FFPROVER_SEARCH_H_
