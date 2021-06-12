//#define DEBUG_MODE
//#define VERBOSE
#define PROFILE

#include <iostream>
#include <algorithm>
#include <random>
#include "utils/ctx.h"
#include "utils/log.h"
#include "utils/types.h"
#include "utils/read_file.h"
#include "lazyparam_prover/controller/prover.h"
#include "lazyparam_prover/controller/features.h"
#include "ffprover/xgboost.h"
#include "ffprover/search.h"
#include "ffprover/tree.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/time.h"

std::mt19937 rnd(7987345);
std::uniform_real_distribution dist(0.,1.);

using namespace ff;

INL static inline FeatureVec action_features(features::ActionVec a) {
  FeatureVec v;
  for(size_t i = 0; i<a.features.v.size(); i++) v.push(i,a.features.v[i]);
  return v;
}
INL static inline FeatureVec state_features(features::StateVec a) {
  FeatureVec v;
  v.push(0,a.proof_size);
  v.push(1,a.open_branches);
  return v;
}

ptr<Model> priority_model;
ptr<Model> reward_model;

INL static inline double logit(double x) {
  if(x==1.) return 10.;
  if(x==0.) return -10;
  auto ret = log(x/(1.-x));
  if(ret<-10.) return -10.;
  if(ret>10.) return 10.;
  return ret;
}

INL static inline double logistic(double v){ return 1./(1.+exp(-v)); }

Search::Config cfg {
  .one_expansion_per_playout = true,
  .playouts_per_bigstep = 10000,
  .playout_depth = 20,
  .base_reward = [](features::StateVec f) {
    const auto value_factor = 0.3;
    return reward_model ? logistic(reward_model->predict(state_features(f))) : value_factor;
  },
  .base_priority = [](features::ActionVec f) {
    const auto policy_temp = 2.;
    return priority_model ? exp(priority_model->predict(action_features(f)))/policy_temp : 1.;
  },
  .child_priority = [](Tree::Ptr t, size_t i) {
    //return dist(rnd) * t.child(i).priority();

    double sum_visits = std::max<size_t>(1,t.visits());
    double visits = std::max<size_t>(1,t.child(i).visits());
    //double factor = sqrt(sum_visits/visits); // UCB no logarithm *)
    //double factor = sqrt(sum_visits)/visits; // PUCB from Alpha Zero *)
    double factor = sqrt(log(sum_visits)/visits); // Original Csaba Szepesvari *)
    const double ucb_const = 1.;
    return (t.child(i).rewards()/visits)
      + ucb_const*t.child(i).priority()*factor;
  },
  .bigstep_selector = [](Tree::Ptr t) {
    const auto navigate_to_win = true; 
    if(auto w = t.won_depth(); navigate_to_win && w) {
      for(size_t i=0; i<t.child_count(); i++) {
        if(auto w2 = t.child(i).won_depth(); w2 && w2.get()==w.get()-1) return i;
      }
      error("inconsistend win_depth");
    }
    ssize_t max_i = -1;
    double max_p = -1;
    for(size_t i=0; i<t.child_count(); i++) {
      auto visits = t.child(i).visits();
      if(visits==0) continue;
      // choose by visits (+rewards tie breaker)
      double p = double(visits) + t.child(i).rewards()/double(visits);
      if(p>max_p){ max_p = p; max_i = i; }
    }
    DEBUG if(max_i==-1) error("no canididate for bigstep");
    return size_t(max_i);
  }
};

ABSL_FLAG(absl::Duration,timeout,absl::Seconds(60),"spend timeout+eps time on searching");
//ABSL_FLAG(uint64_t,max_mem,3000000,"memory limit in bytes");
//ABSL_FLAG(uint64_t,max_infs,20000000,"limit on number of inferences");
//ABSL_FLAG(uint64_t,playout_depth,100,"limit on depth of a single playout");
//ABSL_FLAG(uint64_t,max_bigsteps,100,"limit on number of big steps");

ABSL_FLAG(str,problem_path,"","path to TPTP problem");
ABSL_FLAG(str,priority_model_path,"","path to priority model");
ABSL_FLAG(str,reward_model_path,"","path to reward model");
ABSL_FLAG(str,priority_training_path,"","output path for priority training data");
ABSL_FLAG(str,reward_training_path,"","output path for reward training data");
ABSL_FLAG(size_t,features_space_size,1024,"size of the space that features will be hashed into. Should be <50k");

void save_result(Result res) {
  DataSet priority_data;
  DataSet reward_data;
  bool won = res.status==Result::SOLVED;
  size_t dist = 0;
  for(;res.path.size(); dist++) {
    auto x = res.path.back(); res.path.pop_back();
    reward_data.instances.push_back({
      .label = logit(won?pow(0.98,dist):0.), // TODO: should it really be [-10,10] ?
      .features = state_features(x.state),
    });
    if(won) {
      auto norm_vsum = double(x.tree.visits())/double(x.tree.child_count());
      for(size_t i=0; i<x.tree.child_count(); i++) {
        double v = x.tree.child(i).visits();
        if(v>0) priority_data.instances.push_back({
          .label = std::max<double>(-6,log(v/norm_vsum)),
          .features = action_features(x.actions[i]),
        });
      }
    }
  }
  util::write_file(absl::GetFlag(FLAGS_priority_training_path),str_bytes(priority_data.show()));
  util::write_file(absl::GetFlag(FLAGS_reward_training_path),str_bytes(reward_data.show()));
}

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);
  info("parsed");
  auto ctx = Ctx::with_timeout(Ctx::background(),absl::GetFlag(FLAGS_timeout));
  auto tptp_fof = bytes_str(util::read_file(absl::GetFlag(FLAGS_problem_path)));
  auto problem = controller::Problem::New(tptp_fof);
  auto prover = controller::Prover::New(problem,absl::GetFlag(FLAGS_features_space_size));
  info("loaded problem");
  auto priority_model = Model::New(absl::GetFlag(FLAGS_priority_model_path));
  auto reward_model = Model::New(absl::GetFlag(FLAGS_reward_model_path));
  info("loaded models");

  auto tree = Tree::New();
  Search search(cfg);

  auto res = search.run(ctx,tree->root(),*prover);
  switch(res.status) {
    case Result::SOLVED:
      info("SZS status Theorem");
      break;
      //print_guides init_tree true;
      //print_dot itree
    case Result::DEADEND:
      info("SZS status DeadEnd");
      break;
      //print_guides init_tree false
    case Result::CANCELLED:
      info("SZS status ResourceOut");
      break;
      //print_guides init_tree false
    default:
      error("search.run() = %",res.status);
  }
  for(auto &[n,s] : profile.scopes) {
    info("[prof] % :: count=%, cycles=%, time=%",n,s.count,s.cycles,s.time);
  }
  info("Bigsteps: % Inf: %",search.stats.bigsteps,search.stats.inferences);
  save_result(res);
  return 0;
}
