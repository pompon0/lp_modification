struct _WeakFrame {
  size_t min_cost;
  size_t nodes_limit;
  Branch branch;
};
using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

static bool matches_lemma(State &state, Branch branch) {
  auto a = branch.false_.head();
  auto atom_hash = Index::atom_hash(a);
  for(auto b = branch.true_; !b.empty(); b = b.tail()) {
    if(atom_hash!=Index::atom_hash(b.head())) continue;
    if(state.val.equal_mod_sign(a,b.head())) return true;
  }
  return false;
}

static Frame weak_param(WeakFrame f, Atom L, Term l, Term r) {
  LazyPreWeakConnectionFrame::Builder pb;
  pb->branch = f->branch;
  pb->nodes_limit = f->nodes_limit;
  pb->L = L;
  pb->l = l;
  pb->r = r;
  return Frame(pb.build());
}

// l/=r,...,a = L[p]
template<typename Alts> void try_weak_param(State &state, WeakFrame f, Alts alts,
    Atom x, Atom y, List<Frame> tail) const {
  if(x.pred()!=Atom::EQ || x.sign()) return;
  alts(Cont{weak_param(f,y,x.arg(0),x.arg(1))+tail});
  alts(Cont{weak_param(f,y,x.arg(1),x.arg(0))+tail});
}

// weak connection: -P(r), ..., a = P(s)
template<typename Alts> void try_weak_match(State &state, WeakFrame f, Alts alts,
    Atom x, Atom y, List<Frame> tail) const {
  if(x.pred()==Atom::EQ) return;
  if(Index::atom_hash(x)!=Index::atom_hash(y)) return;
  WeakUnifyFrame::Builder ub;
  ub->a1 = x;
  ub->a2 = y;
  alts(Cont{Frame(ub.build())+tail});
}

template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.false_.head())); 
  state.stats.weak_steps++;
  size_t budget = f->nodes_limit - state.nodes_used;
  COUNTER("expand");
  if(budget<f->min_cost) return;
  List<Frame> tail = frames.tail();
  // match lemma
  if(matches_lemma(state,f->branch)){ alts(Cont{tail}); return; }
  auto a = f->branch.false_.head();
  // reduce:
  if(a.pred()==Atom::EQ && a.sign()) {
    ReductionFrame::Builder b;
    b->a = a;
    alts(Cont{Frame(b.build())+tail});
  }
  // restrict min cost
  if(f->min_cost) {
    MinCostFrame::Builder b;
    b->min_cost = state.nodes_used + f->min_cost;
    tail += Frame(b.build());
  }
  // weak connections
  for(auto b = f->branch.false_.tail(); !b.empty(); b = b.tail()) {
    try_weak_match(state,f,alts,a,b.head(),tail);
    try_weak_param(state,f,alts,a,b.head(),tail);
    try_weak_param(state,f,alts,b.head(),a,tail);
  }
  // strong connections
  // P(r),-P(s)
  if(a.pred()!=Atom::EQ) {
    auto matches = state.cla_index.get_matches(a,budget);
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branch;
      b->dcla = ca.cla;
      b->strong_id = ca.i;
      alts(Cont{Frame(b.build()) + frames.tail()});
    }
  }
  // L[p],l/=r
  auto matches = state.cla_index.get_matches(Atom::EQ,false,budget);
  while(auto mca = matches.next()) {
    auto ca = mca.get();
    DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
    LazyPreStrongConnectionFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branch;
    b->dcla = ca.cla;
    b->strong_id = ca.i;
    b->branch_lr = false;
    alts(Cont{Frame(b.build()) + frames.tail()});
  }
  if(a.pred()==Atom::EQ && !a.sign()) {
    // l/=r,L[p]
    auto matches = state.cla_index.get_all(budget);
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
      LazyPreStrongConnectionFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branch;
      b->dcla = ca.cla;
      b->strong_id = ca.i;
      b->branch_lr = true;
      alts(Cont{Frame(b.build()) + frames.tail()});
    }
  }
}
