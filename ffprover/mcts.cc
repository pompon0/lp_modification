#include <iostream>
#include <algorithm>
#include "utils/ctx.h"
#include "utils/log.h"
#include "utils/types.h"
#include "utils/read_file.h"
#include "lazyparam_prover/controller.h"
#include "ffprover/xgboost.h"
#include "ffprover/search.h"
#include "ffprover/tree.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/time.h"

using namespace ff;

ptr<Model> priority_model;
ptr<Model> reward_model;

INL static inline double logistic(double v){ return 1./(1.+exp(-v)); }

INL static inline FeatureVec action_features(controller::ActionFeaturesVec a){ return {}; }
INL static inline FeatureVec state_features(controller::StateFeaturesVec a){ return {}; }

Search::Config cfg {
  .one_expansion_per_playout = true,
  .playouts_per_bigstep = 2000,
  .playout_depth = 200,
  .base_reward = [](controller::StateFeaturesVec f) {
    const auto value_factor = 0.3;
    return reward_model ? logistic(reward_model->predict(state_features(f))) : value_factor;
  },
  .base_priority = [](controller::ActionFeaturesVec f) {
    const auto policy_temp = 2.;
    return priority_model ? exp(priority_model->predict(action_features(f)))/policy_temp : 1.;
  },
  .child_priority = [](Tree::Ptr t, size_t i) {
    //return Random.float(1.) * prior;;

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

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);
  info("parsed");
  auto ctx = Ctx::with_timeout(Ctx::background(),absl::GetFlag(FLAGS_timeout));
  auto tptp_fof = bytes_str(util::read_file(absl::GetFlag(FLAGS_problem_path)));
  auto problem = controller::Problem::New(tptp_fof);
  auto prover = controller::Prover::New(problem);
  info("loaded problem");
  auto priority_model = Model::New(absl::GetFlag(FLAGS_priority_model_path));
  auto reward_model = Model::New(absl::GetFlag(FLAGS_reward_model_path));
  info("loaded models");

  auto tree = Tree::New();
  Search search(cfg);

  auto res = search.run(ctx,tree->root(),*prover);
  switch(res) {
    case Search::SOLVED:
      info("SZS status Theorem");
      break;
      //print_guides init_tree true;
      //print_dot itree
    case Search::DEADEND:
      info("SZS status DeadEnd");
      break;
      //print_guides init_tree false
    case Search::CANCELLED:
      /*if(is_theorem==[]){
        printf "%% SZS status ResourceOut: %s\n%!" x;
        print_guides init_tree false
      } else {
        printf "%% SZS status Theorem (slow)\n%%";
        print_guides init_tree true
      }*/
      info("SZS status ResourceOut");
      break;
      //print_guides init_tree false
    default:
      error("search.run() = %",res);
  }
  //printf "%% Proof: %s\n" (String.concat " " (List.map string_of_int (List.rev !bigstep_hist)));
  info("%% Bigsteps: % Inf: % %%",search.stats.bigsteps,search.stats.inferences);
  return 0;
}
