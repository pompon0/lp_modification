struct _MinCostFrame { size_t min_cost; };
using MinCostFrame = Variant<Frame,Frame::MIN_COST,_MinCostFrame>;
void min_cost(MinCostFrame f) const { FRAME("min_cost");
  state.stats.min_cost_steps++;
  List<Cont> alts;
  if(state->nodes_used>=f->min_cost) alts += builder.build();
  return alts;
}
