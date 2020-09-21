struct _WeakFrame {
  size_t min_cost;
  size_t nodes_limit;
  Branch branch;
};
using WeakFrame = memory::Variant<Frame,Frame::WEAK,_WeakFrame>;

static bool matches_lemma(State &state, Branch branch) {
  auto a = branch.false_.head();
  auto atom_hash = Index::atom_hash(a);
  for(auto b = branch.true_; !b.empty(); b = b.tail()) {
    if(atom_hash!=Index::atom_hash(b.head())) continue;
    if(state.val.equal_mod_sign(a,b.head())) return true;
  }
  return false;
}

// l/=r,...,a = L[p]
memory::List<Cont> try_weak_param(memory::Alloc &A, WeakFrame f, memory::List<Cont> alts,
    Atom lr, Atom L, Builder tail) const {
  if(lr.pred()!=Atom::EQ || lr.sign()) return alts;
  LazyPreWeakConnectionFrame::Builder pb(A);
  pb->branch = f->branch;
  pb->nodes_limit = f->nodes_limit;
  pb->L = L;
  pb->lr = lr;
  return alts.add(A,tail.add(A,Frame(pb.build())).build());
}

// weak connection: -P(r), ..., a = P(s)
memory::List<Cont> try_weak_match(memory::Alloc &A, WeakFrame f, memory::List<Cont> alts,
    Atom x, Atom y, Builder tail) const {
  if(x.pred()==Atom::EQ) return alts;
  if(Index::atom_hash(x)!=(Index::atom_hash(y)^1)) return alts;
  WeakUnifyFrame::Builder ub(A);
  ub->a1 = x;
  ub->a2 = y;
  return alts.add(A,tail.add(A,Frame(ub.build())).build());
}

memory::List<Cont> weak(memory::Alloc &A, WeakFrame f) const { STATE_FRAME(A,state,"weak(%)",show(f->branch.false_.head())); 
  state->stats.weak_steps++;
  size_t budget = f->nodes_limit - state->nodes_used;
  COUNTER("expand");
  if(budget<f->min_cost) return memory::nothing();
  Builder tail = builder();
  memory::List<Cont> alts;
  // match lemma
  if(matches_lemma(*state,f->branch)){ 
    STATE_FRAME(A,state,"matches_lemma");
    return alts.add(A,tail.build());
  }
  auto a = f->branch.false_.head();
  // reduce:
  if(a.pred()==Atom::EQ && a.sign()) {
    ReductionFrame::Builder b(A);
    b->a = a;
    alts.push(A,tail.add(A,Frame(b.build())).build());
  }
  // restrict min cost
  if(f->min_cost) {
    MinCostFrame::Builder b(A);
    b->min_cost = state->nodes_used + f->min_cost;
    tail = tail.add(A,Frame(b.build()));
  }
  // weak connections
  for(auto b = f->branch.false_.tail(); !b.empty(); b = b.tail()) {
    alts = try_weak_match(A,f,alts,a,b.head(),tail);
    alts = try_weak_param(A,f,alts,a,b.head(),tail);
    alts = try_weak_param(A,f,alts,b.head(),a,tail);
  }
  // strong connections
  // P(r),-P(s)
  if(a.pred()!=Atom::EQ) {
    STATE_FRAME(A,state,"[%] %",a.id(),show(a));
    // since a may have been paramodulated, we cannot use the most optimized
    // version of get_matches.
    auto matches = state->cla_index.get_matches(a.pred(),!a.sign(),budget);
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
      StrongFrame::Builder b(A);
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branch;
      b->dcla = ca.cla;
      b->strong_id = ca.i;
      alts.push(A,builder().add(A,Frame(b.build())).build());
    }
  }
  // L[p],l/=r
  auto matches = state->cla_index.get_matches(Atom::EQ,false,budget);
  while(auto mca = matches.next()) {
    auto ca = mca.get();
    DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
    LazyPreStrongConnectionFrame::Builder b(A);
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branch;
    b->dcla = ca.cla;
    b->strong_id = ca.i;
    b->branch_lr = false;
    alts.push(A,builder().add(A,Frame(b.build())).build());
  }
  if(a.pred()==Atom::EQ && !a.sign()) {
    // l/=r,L[p]
    auto matches = state->cla_index.get_all(budget);
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
      LazyPreStrongConnectionFrame::Builder b(A);
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branch;
      b->dcla = ca.cla;
      b->strong_id = ca.i;
      b->branch_lr = true;
      alts.push(A,builder().add(A,Frame(b.build())).build());
    }
  }
  return alts;
}
