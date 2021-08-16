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

  Search(Config _cfg) : cfg(_cfg) {}

  Config cfg;
  Result::Stats stats;

  Result run(Ctx::Ptr ctx, Tree::Ptr t, controller::Prover &p) { FRAME("Search::run");
    PROF_TIME("Search::run");
    while(!ctx->done()) {
      info("bigsteps = %",stats.bigsteps);
      // early exit
      if(p.done()) return {.status = Result::SOLVED, .node = t, .stats = stats};
      if(t.lost()) return {.status = Result::DEADEND, .node = t, .stats = stats};
      // perform playouts
      auto s = p.save();
      for(size_t i = 0; !ctx->done() && i<cfg.playouts_per_bigstep; i++) {
        playout(t,p);
        p.restore(s);
      }
      if(ctx->done()) break;
      auto i = cfg.bigstep_selector(t);
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

  void expand(Tree::Ptr t, controller::Prover &p) {
    size_t n = p.action_count();
    vec<double> priorities(n);
    for(size_t i=0; i<n; i++) priorities[i] = cfg.base_priority(p.action_features(i));
    t.expand(priorities);
  }

  size_t select_child(Tree::Ptr t) {
    if(t.child_count()==0) error("t has no children");
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
        expansions++;
        expand(t,p);
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

struct Output {
  DataSet priority_data;
  DataSet reward_data;
};

static ptr<Output> collect_output(Tree::Ptr t, controller::Prover &prover, double reward) {
  if(!t.has_parent()) return make<Output>();
  auto output = collect_output(t.parent(),prover,reward*0.98); 
  size_t i = t.parent().child_id(t);
  auto norm_vsum = double(t.parent().visits())/double(t.parent().child_count());
  for(size_t i=0; i<t.parent().child_count(); i++) {
    double v = t.parent().child(i).visits();
    if(v>0) output->priority_data.instances.push_back({
      .label = std::max<double>(-6,log(v/norm_vsum)), // this is dual to the exp in the base_priority
      .features = action_features(prover.action_features(i)),
    });
  }
  prover.apply_action(i);
  output->reward_data.instances.push_back({
    .label = logit(reward), // TODO: should it really be [-10,10] ?
    .features = state_features(prover.state_features()),
  });
  return output;
}

}  // namespace ff

#endif  // FFPROVER_SEARCH_H_
