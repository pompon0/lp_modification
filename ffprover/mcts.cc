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

/*

double logit(x) {
  if(x==1.) return 10.;
  if(x==0.) return -10;
  auto ret = log(x/(1.-x));
  if(ret<-10.) return -10.;
  if(ret>10.) return 10.;
  return ret;
}

double logistic(double v){ return 1./(1.+exp(-v)); }

struct Tree {
  enum Kind { Open, Unexplored, Lost, Won };
  Kind kind;
  Tree *parent;
  double prior; // prediction value
  double wins; // W = wins
  int visits; // N = visit count
  vec<Tree> branches; // subtrees for actions
  double reward;

  void fail() {
    if(kind!=Lost) closed++;
    kind = Lost;
  }

  double ucb(double sum_visits) {
    double visits = max(1.0,visits);
    souble sum_visits = max(1.0,sum_visits);
    switch(ucb_mode) {
    case 1: factor = sqrt(sum_visits/visits); break; // UCB no logarithm *)
    case 2: factor = sqrt(sum_visits/visits); break; // PUCB from Alpha Zero *)
    default: factor = sqrt(log(sum_visits)/visits); break; // Original Csaba Szepesvari *)
    }
    if(do_ucb) return (wins/visits) + ucb_const*prior*factor;
    return Random.float(1.) * prior;;
  }

  size_t get_rel() {
    size_t max_i = 0;
    double max_u = -1;
    for(size_t i=0; i<branches.size(); i++) {
      if(branches[i].kind==Lost) continue;
      auto u = branches[i].ucb(visits);
      if(u>max_u){ max_u = u; max_i = i; }
    }
    return max_i;
  }
};

// Shortest proof so far
Tree *is_theorem = 0;
// Main history otherwise *)
vec<Tree> bigstep_trees = {};

template<T> (vec<T>,vec<T>) splitl(vec<T> l) {
  // split list into even and odd elements
}

auto nnumber = 0;

// Some counters *)
auto bigsteps = 0;
auto opened = 0;
auto closed = 0;
auto edges = 0;
auto totfea = 0;


vec<double> priors(State st) {
  edges += st.actions.size();
  if(!predict_policy) {
    return st.actions.map(_ => 1.);
  }
  vec<vec<int,int>> fealist;
  for(a : actions) {
    fealist.push_back(p->get_action_features(a));
    totfea += fealist.back().size();
  }
  auto predicts = Xgb.predict_p(fealist);
  for(auto &p : predics) p = exp(p/policy_temp);
  return predicts;
}

vec<double> normalize(vec<double> l) {
  double s = 0; for(auto x : l) s += x;
  for(auto &x : l) x /= s;
  return l;
}

void do_tree(Tree &tree, const State &st) {
  if(st.win==1) {
    tree.kind = Won;
    if(!is_theorem || is_theorem.size() > tree.depth() + 1) {
      is_theorem = tree;
      max_infs = 1000000000;
      if(thm_play_count >= 0) play_count = thm_play_count;
    }
  } else if(st.win==-1 || st.actions==[]) {
    tree.fail();
  }
  switch(tree.kind) {
    case Tree::Won:
    case Tree::Lost:
      return;
    case Open:
      // check if not all branches are lost.
      for(Tree &x : tree.branches) if(x.kind!=Tree::Lost) return;
      fail(tree);
      return;
    case Unexplored:
      opened++;
      if(!one_per_play) tree.kind = Tree::Open;
      for(auto p : normalize(priors(st))) {
        tree.branches.push_back({
          .kind = Tree::Unexplored,
          .prio = p,
          .wins = 0.,
          .visits = 0.,
          .branches = [],
          .reward = 0.,
        });
      }
      return;
  }
}

double reward(State st, Tree tree) {
  if(tree.kind==Tree::Won) return 1;
  if(tree.kind==Tree::Lost) return 0.;
  if(predict_value) {
    auto f = st.p->get_state_features();
    totfea += f.size();
    return logistic(Xgb.predict_v(f));
  }
  return value_factor;
}

enum Status { Solved, DeadEnd, ResourceOut };

Maybe<Status> playout(int depth, Tree tree, State st) {
  while(depth-- && tree.kind==Tree::Open) {
    if(auto ms = check_limits()) return ms.get();
    infer++;
    auto i = tree.get_rel();
    tree = tree.branches[i];
    st.move(st.acts[i]);
    do_tree(tree,st);
  }
  if(tree.kind==Unexplored) tree.kind = Tree::Open;
  tree.reward = reward(st,tree);
  for(Tree *x := tree; x!=0; x = x.parent) {
    x.wins += tree.reward;
    x.visits++;
  }
  return nothing();
}

auto bigstep_hist = [];

Maybe<Status> check_limits(Ctx ctx) {
  if(ctx.done()) return just(ResourceOut); // error("max_time");
  if(infer>=max_infs) return just(ResourceOut); // error("max_infs");
  if(Xgb.c_mem()>=max_mem) return just(ResourceOut); // error("max_mem");
  return nothing();
}

Status bigstep(State st, Tree tree) {
  while(1) {
    if(tree.kind==Tree::Unexplored) tree.kind = Open;  // (* freshly visited *)
    for(size_t i = 0; i<play_count; i++) {
      playout(play_dep,st,tree);
      if(auto ms = check_limits()) return ms.get();
    }
    if(tree.kind==Tree::Won) return Solved;
    if(tree.kind==Tree::Lost) return DeadEnd;
    size_t max_i = 0;
    if(thm_play_count == -1 || !is_theorem) {
      double max_p = -1;
      for(size_t i=0; i<tree.branches.size(); i++) {
        auto *t = tree.branches[i];
        if(t.visits==0) continue;
        double p = t.visits + t.wins/double(t.visits);
        if(p>max_p){ max_p = p; max_i = i; }
      }
    } else {
      for(size_t i=0; i<tree.branches.size(); i++ {
        auto *t = tree.branches[i];
        if(t.kind==tree::Won){ max_i = i; break; }
        for(Tree *x = is_theorem; x!=0 && x!=t; x = x.parent) {
        }
        if(x==t){ max_i = i; break; }
      }
    }
    bigsteps++;
    bigstep_hist += i;
    bigstep_trees += tree;
    tree = tree.branches[i];
    st.move(st.actions[i]);
    do_tree(tree,st);
  }
}*/

/*
auto thm_play_count = 0;
auto play_count = 2000;
auto one_per_play = true;
auto ucb_mode = 0;
auto do_ucb = true;
auto play_dep = 1000;
auto ucb_const = 1.;
auto value_factor = 0.3;
auto save_above = -1;
auto predict_value = true;
auto predict_policy = true;
auto policy_temp = 2.;
*/
ABSL_FLAG(absl::Duration,timeout,absl::Seconds(60),"spend timeout+eps time on searching");
//ABSL_FLAG(uint64_t,max_mem,3000000,"memory limit in bytes");
ABSL_FLAG(uint64_t,max_infs,20000000,"limit on number of inferences");
ABSL_FLAG(str,problem_path,"","path to TPTP problem");
ABSL_FLAG(str,policy_model_path,"","path to policy model");
ABSL_FLAG(str,value_model_path,"","path to value model");


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
  auto policy_model = ff::Model::New(absl::GetFlag(FLAGS_policy_model_path));
  auto value_model = ff::Model::New(absl::GetFlag(FLAGS_value_model_path));
  info("loaded models");

  // TODO: set SIGINT i SIGTERM to exit by throwing exception
  // Initial tree with one unexplored node *)
  /*
  Tree itree {
    .kind = Tree::Unexplored,
    .prior = 1.,
    .wins = 0.,
    .visits = 0,
    .branches = [],
    .reward = 0.,
  };
  auto init_tree = do_tree(itree,init_state);
  try {
    playout(play_dep,init_tree);
    bigstep(init_tree);
  }
  catch(Solved) {
    printf "%% SZS status Theorem (fast)\n%!";
    print_guides init_tree true;
    print_dot itree
  }
  catch(Failure x) {
    printf "%% SZS status Error\n%%%s\n%!" x
  }
  catch(DeadEnd){
    printf "%% SZS status DeadEnd\n%!";
    print_guides init_tree false
  }
  catch(ResourceOut x){
    if(is_theorem==[]){
      printf "%% SZS status ResourceOut: %s\n%!" x;
      print_guides init_tree false
    } else {
      printf "%% SZS status Theorem (slow)\n%%";
      print_guides init_tree true
    }
  }
  catch(Parsing.Parse_error) {
    printf "%% SZS status Error\n%%Parse_error\n%!";
  }
  printf "%% Proof: %s\n" (String.concat " " (List.map string_of_int (List.rev !bigstep_hist)));
  printf "%% Bigsteps: %i Inf: %i Op: %i Cl: %i Ed:%i TotFea:%i Tim:%f\n" !bigsteps !infer !opened !closed !edges !totfea (Sys.time () -. start_time);
  */
  return 0;
}
