struct _WeakFrame { size_t min_cost; size_t nodes_limit; Branch branch; };
using WeakFrame = memory::Variant<Frame,Frame::WEAK,_WeakFrame>;

INL memory::List<Cont> weak(memory::Alloc &A, WeakFrame f) const { FRAME("weak(%)",show(f->branch.false_.head())); 
  state->stats.weak_steps++;
  size_t budget = f->nodes_limit - state->nodes_used;
  COUNTER("expand");
  if(budget<f->min_cost) return memory::nothing();
  auto tail = builder();
  if(f->min_cost) {
    MinCostFrame::Builder b(A);
    b->min_cost = state->nodes_used + f->min_cost;
    tail = tail.add(A,Frame(b.build()));
  }
  
  auto matches = state->cla_index.get_matches(f->branch.false_.head(),budget);
  memory::List<Cont> alts;
  while(auto mca = matches.next()) {
    auto ca = mca.get();
    DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
    StrongFrame::Builder b(A);
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branch;
    b->dcla = ca.cla;
    b->strong_id = ca.i;
    alts.push(A,tail.add(A,Frame(b.build())).build());
  }
  return alts;
}
