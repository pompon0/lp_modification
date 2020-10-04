#include "lazyparam_prover/types.h"

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

auto max_time = 60. // seconds
auto max_mem = 3000000;
auto max_infs = 20000000;

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


void print_guides(init_tree, bool won) {
  auto do_seq(ostream &oc, ps) {
    for(auto [i,n] : ps) oc << util::fmt(" %:%",i,n);
  }
  auto (st, t, _, al) = init_tree;

  let ts = won ? is_theorem : bigstep_trees;
  st.p->restore(0);
  auto ocv = ofstream(path_v);
  auto ocp = ofstream(path_p);
  size_t l = ts.size()-1; 
  auto print_more(Prover p, Tree t, int dist, State st, vec<Action> al) {
    if(dist==0) return;
    if(t.kind!=Tree::Open) error("aaa");
    p->fea_global_update();
    auto f = p->get_fea();
    ocv << "%.5f" (logit (won ? pow(0.98,dist) : 0.));
    do_seq(ocv,f); ocv << "\n";
    if(won) {
      vec<auto> fealist;
      for(auto a : al) fealist.push_back(p->get_act_fea(a));
      auto norm_vsum = double(t.n)/t.b.size();
      for(size_t i=0; i<t.b.size(); i++) {
        auto t = t.b[i];
        auto f = fealist[i];
        if(0<t.n) {
          auto p = max(-6,log(double(t.n)/norm_vsum));
          ocp << "%.5f" p;
          do_seq(ocp,f);
          ocp << "\n";
        }
      }
    }
    auto nt,na;
    bool ok = false;
    for(size_t i=0; i<t.b.size(); i++) {
      for(auto x : ts) if(t.b[i]==x){ ok = true; nt = t.b[i]; na = al[i]; }
    }
    if(!ok) error("not_found");
    auto [_, (ns, al)] = extend(st,na,in);
    print_more(p,nt,dist - 1,ns,al);
  }
  print_more(p,t,l,st,al);
  ocv.close();
  ocp.close_out();
}

auto bigstep_hist = [];

Maybe<Status> check_limits() {
  if(infer>=max_infs) return just(ResourceOut); // error("max_infs");
  if(Sys.time()-start_time>=max_time) return just(ResourceOut); // error("max_time");
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
}

void main(int argc, char **argv) {
  auto start_time = Sys.time();
  /* TODO: set SIGINT i SIGTERM to exit by throwing exception */
  Xgb.init (predict_policy || predict_value);;
  auto init_state = Ff.start(argv[1]);
  // Initial tree with one unexplored node *)
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
}
