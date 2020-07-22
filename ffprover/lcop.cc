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

auto max_time = 60.
auto max_mem = 3000000;
auto max_infs = 20000000;

struct Tree {
  enum Kind { Open, Unexplored, Lost, Won };
  Kind kind;
  double p; // prediction value
  double w; // W = wins
  int n; // N = visit count
  vec<Tree> b; // subtrees for actions
  double r;
};

// Shortest proof so far
Proof is_theorem = [];
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


vec<double> priors(Prover p, vec<Action> actions) {
  edges += actions.size();
  if(!predict_policy) {
    return actions.map(_ => 1.);
  }
  p->fea_global_update();
  
  auto fealist;
  for(a : actions) {
    auto back = p->save();
    auto subst_before = p->subst_hist.size();
    p->act(a);
    p->fea_action_update(subst_before);
    fealist.push_back(p->get_action_features());
    p->restore(back);
    totfea += fealist.back().size();
  }
  auto predicts = Xgb.predict_p(fealist);
  for(auto &p : predics) p = exp(p/policy_temp);
  return predicts;
}

// Initial tree with one unexplored node *)
Tree itree {
  .kind = Tree::Unexplored,
  .p = 1.,
  .w = 0.,
  .n = 0,
  .b = [],
  .r = 0.,
};

vec<double> normalize(vec<double> l) {
  double s = 0; for(auto x : l) s += x;
  for(auto &x : l) x /= s;
  return l;
}

auto fail(Tree tree) {
  if(tree.kind!=Tree::Lost) {
    closed++;
    tree.kind = Tree::Lost;
  }
}

auto do_tree(tree,thist,(i, (st, acts))) {
  if(i==1) {
    tree.kind := Won;
    if(is_theorem==[] || is_theorem.size() > thist.size() + 1) {
      is_theorem = List(tree,thist);
      auto max_infs = 1000000000;
      if(thm_play_count >= 0) play_count = thm_play_count;
    }
    return (st, tree, tree :: thist, []);
  }
  if(i==-1 || acts==[]) {
    fail(tree);
    return (st, tree, tree :: thist, [])
  } 
  switch(tree.kind) {
    case Tree::Won:
    case Tree::Lost:
      return (st, tree, tree :: thist, []);
    case Open:
      for(auto x : tree.b) if(x.kind!=Tree::Lost) 
        return (st, tree, tree :: thist, acts);
      fail(tree);
      return (st, tree, tree :: thist, []);
    case Unexplored:
      opened++;
      auto l = normalize(priors(st,acts));
      auto b = List.map (fun p -> {kind=Unexplored; p; w=0.; n=0; b=[]; r=0.}) l in
      if(not one_per_play) tree.kind = Open;
      tree.b = b;
      return (st, tree, tree :: thist, acts);;
  }
}

// 'arg_max get_val l' computes the _index_ of element of list l which has maximal get_val *)
size_t arg_max(get_val,vec<T> l) {
  if(l.size()==0) error("arg_max: empty list");
  int res = MIN_INT;
  size_t idx = 0;
  for(size_t i=0; i<l.size(); i++) if(get_val(h)>res) idx = i;
  return idx;
}

auto ucb(double sum_visits, double prior, double wins, double visits) {
  auto visits = max(1.0,visits);
  auto sum_visits = max(1.0,sum_visits);
  switch(ucb_mode) {
  case 1: factor = sqrt(sum_visits/visits); break; // UCB no logarithm *)
  case 2: factor = sqrt(sum_visits/visits); break; // PUCB from Alpha Zero *)
  default: factor = sqrt(log(sum_visits)/visits); break; // Original Csaba Szepesvari *)
  }
  if(do_ucb) return (wins/visits) + ucb_const*prior*factor;
  return Random.float(1.) * prior;;
}

double get_rel(sum_visits,Tree t) {
  if(tree.kind==Tree::Lost) return -1;
  return ucb(sum_visits,t.p,t.w,t.n);
}

double logistic(double v){ return 1./(1.+exp(-v)); }

double reward(Prover *p, Tree tree) {
  if(tree.kind==Tree::Won) return 1;
  if(tree.kind==Lost) return 0.;
  if(predict_value) {
    p->fea_global_update();
    auto f = p->get_fea();
    totfea += f.size();
    return logistic (Xgb.predict_v(f));
  }
  return value_factor;
}

auto start_time = Sys.time();

void playout(int depth,(st, Tree tree, vec<Tree> thist, vec<Action> acts)) {
  check_limits();
  if(tree.kind==Tree::Open && depth >= 0) {
    auto i = arg_max(get_rel(tree.n,_),tree.b);
    infer++;
    return playout(depth - 1, do_tree, tree.b[i], thist, Ff.extend(st,acts[i]));
  }
  if(tree.kind==Unexplored) tree.kind = Open;
  tree.r = reward(st.sr.p,tree);
  for(auto &t : thist){ t.w += tree.r; t.n++; }
}

double logit(x) {
  if(x==1.) return 10.;
  if(x==0.) return -10;
  auto ret = log(x/(1.-x));
  if(ret<-10.) return -10.;
  if(ret>10.) return 10.;
  return ret;
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

enum Status { Solved, DeadEnd, ResourceOut };

Maybe<Status> check_limits() {
  if(infer>=max_infs) return just(ResourceOut); // error("max_infs");
  if(Sys.time()-start_time>=max_time) return just(ResourceOut); // error("max_time");
  if(Xgb.c_mem()>=max_mem) return just(ResourceOut); // error("max_mem");
  return nothing();
}

Status bigstep((st, Tree tree, vec<Tree> thist, acts) as state) {
  while(1) {
    if(tree.kind==Tree::Unexplored) tree.kind = Open;  // (* freshly visited *)
    for(size_t i = 0; i<play_count; i++) {
      playout(play_dep,state);
      if(auto ms = check_limits()) return ms.get();
    }
    if(tree.kind==Tree::Won) return Solved;
    if(tree.kind==Tree::Lost) return DeadEnd;
    auto i = (is_theorem==[] || thm_play_count == -1) ? 
      (arg_max (fun t -> t.n==0 ? 0 : t.n + t.w/double(t.n)) tree.b)
      (arg_max (fun t -> t.kind==tree::Won ? 2 : is_theorem.contains(t) ? 1 : 0) tree.b);
    bigsteps++;
    bigstep_hist += i;
    bigstep_trees += tree;
    (st,tree,thist,acts) = do_tree(tree.b[i],thist,Ff.extend(st,acts[i]));
  }
}

void main(int argc, char **argv) {
  /* TODO: set SIGINT i SIGTERM to exit by throwing exception */
  Xgb.init (predict_policy || predict_value);;
  auto init_state = Ff.start(argv[1]);
  init_state.sr.p->fea_init(0);
  auto init_tree = do_tree(itree,[],init_state);
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
