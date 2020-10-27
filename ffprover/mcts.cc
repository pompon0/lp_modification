#include <iostream>
#include "utils/ctx.h"
#include "utils/log.h"
#include "utils/types.h"
#include "utils/read_file.h"
#include "lazyparam_prover/controller.h"
#include "ffprover/xgboost.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/time.h"


static inline double logit(double x) {
  if(x==1.) return 10.;
  if(x==0.) return -10;
  auto ret = log(x/(1.-x));
  if(ret<-10.) return -10.;
  if(ret>10.) return 10.;
  return ret;
}

static inline double logistic(double v){ return 1./(1.+exp(-v)); }

struct Search {
  // [0,1]
  double reward(Tree:Ptr t, controller::Prover &p) {
    if(p.done()) return 1.;
    if(t.lost()) return 0.;
    return predict_value ? logistic(reward_model.predict(p.state_features())) : value_factor;
  }

  void expand(Tree::Ptr t, controller::Prover &p) {
    size_t n = p.action_count();
    vec<double> priorities(n);
    for(size_t i=0; i<n; i++) priorities[i] = priority_model.predict(exp(p.action_features(i)/policy_temp));
    t.expand(priorities);
  }

  void playout(int depth, Tree::Ptr t, controller::Prover &p) {
    size_t expansions = 0;
    while(depth--) {
      if(p.done()) break;
      if(!t.expanded()) {
        if(expansions && one_per_play) break;
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

  auto bigstep_hist = [];

  /*Maybe<Status> check_limits(Ctx ctx) {
    if(ctx.done()) return just(ResourceOut); // error("max_time");
    if(infer>=max_infs) return just(ResourceOut); // error("max_infs");
    if(Xgb.c_mem()>=max_mem) return just(ResourceOut); // error("max_mem");
    return nothing();
  }*/

  Status run(Ctx ctx, Tree::Ptr t, controller::Prover &p) {
    while(!ctx.done()) {
      auto s = p.save();
      for(size_t i = 0; i<play_count; i++) {
        p.reset(s);
        playout(t,p);
      }
      if(p.done()) return Solved;
      if(t.lost()) return DeadEnd;
      size_t max_i = 0;
      if(navigate_to_win && t.win_depth()) {
        for(size_t i=0; i<t.children.size(); i++ {
          if(t.child(i).win_depth==t.win_depth-1){ max_i = i; break; }
        }
      } else {
        double max_p = -1;
        for(size_t i=0; i<tree.branches.size(); i++) {
          auto *t = tree.branches[i];
          if(t.visits==0) continue;
          // choose by visits (+rewards tie breaker)
          double p = double(t.visits) + t.rewards/double(t.visits);
          if(p>max_p){ max_p = p; max_i = i; }
        }
      }
      stats.bigsteps++;
      tree = tree.branches[i];
      st.move(st.actions[i]);
    }
    return TimeOut;
  }
};

/*
auto thm_play_count = 0;
auto play_count = 2000;
auto one_per_play = true;

auto do_ucb = true;
auto ucb_mode = 0;
auto ucb_const = 1.;

auto play_dep = 1000;
auto save_above = -1;
auto predict_value = true; // reward = predict_value ? logistic(reward_model) : 0.3 
auto predict_policy = true; // priority = normalize(predict_policy ? exp(priority_model/policy_temp) : 1)

auto value_factor = 0.3;
auto policy_temp = 2.;
*/
ABSL_FLAG(absl::Duration,timeout,absl::Seconds(60),"spend timeout+eps time on searching");
//ABSL_FLAG(uint64_t,max_mem,3000000,"memory limit in bytes");
ABSL_FLAG(uint64_t,max_infs,20000000,"limit on number of inferences");
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
  auto priority_model = ff::Model::New(absl::GetFlag(FLAGS_priority_model_path));
  auto reward_model = ff::Model::New(absl::GetFlag(FLAGS_reward_model_path));
  info("loaded models");

  // TODO: set SIGINT i SIGTERM to exit by throwing exception
  auto tree = Tree::New();

  switch(bigstep(ctx,tree->root(),*prover)) {
    case Solved:
      printf "%% SZS status Theorem (fast)\n%!";
      print_guides init_tree true;
      print_dot itree
    case Failure:
      printf "%% SZS status Error\n%%%s\n%!" x
    case DeadEnd:
      printf "%% SZS status DeadEnd\n%!";
      print_guides init_tree false
    case ResourceOut:
      if(is_theorem==[]){
        printf "%% SZS status ResourceOut: %s\n%!" x;
        print_guides init_tree false
      } else {
        printf "%% SZS status Theorem (slow)\n%%";
        print_guides init_tree true
      }
  }
  printf "%% Proof: %s\n" (String.concat " " (List.map string_of_int (List.rev !bigstep_hist)));
  printf "%% Bigsteps: %i Inf: %i Ed:%i TotFea:%i Tim:%f\n" !bigsteps !infer !edges !totfea (Sys.time () -. start_time);
  return 0;
}
