
static inline double logit(double x) {
  if(x==1.) return 10.;
  if(x==0.) return -10;
  auto ret = log(x/(1.-x));
  if(ret<-10.) return -10.;
  if(ret>10.) return 10.;
  return ret;
}

auto nnumber = 0;

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
