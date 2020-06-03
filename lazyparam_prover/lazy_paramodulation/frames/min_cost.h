struct _MinCostFrame { size_t min_cost; };
using MinCostFrame = Variant<Frame,Frame::MIN_COST,_MinCostFrame>;

template<typename Alts> void min_cost(State &state, MinCostFrame f, Alts &alts) const { STATE_FRAME(state,"min_cost");
  state.stats.min_cost_steps++;
  if(state.nodes_used>=f->min_cost) alts(Cont{frames.tail()});
}
