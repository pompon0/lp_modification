struct _WeakFrame { size_t min_cost; size_t nodes_limit; Branch branch; };
using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.false_.head())); 
  state.stats.weak_steps++;
  size_t budget = f->nodes_limit - state.nodes_used;
  COUNTER("expand");
  if(budget<f->min_cost) return;
  List<Frame> tail = frames.tail();
  if(f->min_cost) {
    MinCostFrame::Builder b;
    b->min_cost = state.nodes_used + f->min_cost;
    tail += Frame(b.build());
  }
  
  // TODO: select strong connection
  /*auto matches = state.cla_index.get_matches(f->branch.false_.head(),budget);
  while(auto mca = matches.next()) {
    auto ca = mca.get();
    DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
    StrongFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branch;
    b->dcla = ca.cla;
    b->strong_id = ca.i;
    alts(Cont{Frame(b.build()) + tail});
  }*/
}
