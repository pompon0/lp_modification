struct _WeakFrame { size_t min_cost; size_t nodes_limit; Branch branch; };
using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

List<Cont> weak(WeakFrame f) const { FRAME("weak(%)",show(f->branch.false_.head())); 
  state->stats.weak_steps++;
  size_t budget = f->nodes_limit - state->nodes_used;
  COUNTER("expand");
  if(budget<f->min_cost) return nothing();
  auto tail = builder();
  if(f->min_cost) {
    MinCostFrame::Builder b(state->A);
    b->min_cost = state->nodes_used + f->min_cost;
    tail = tail.add(Frame(b.build()));
  }
  
  auto matches = state->cla_index.get_matches(f->branch.false_.head(),budget);
  List<Cont> alts;
  while(auto mca = matches.next()) {
    auto ca = mca.get();
    DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
    StrongFrame::Builder b(state->A);
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branch;
    b->dcla = ca.cla;
    b->strong_id = ca.i;
    alts.push(state->A,tail.add(Frame(b.build())).build());
  }
  return alts;
}
