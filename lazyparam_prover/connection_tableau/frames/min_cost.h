struct _MinCostFrame { size_t min_cost; };
using MinCostFrame = Variant<Frame,Frame::MIN_COST,_MinCostFrame>;
List<Cont> min_cost(memory::Alloc &A, MinCostFrame f) const { FRAME("min_cost");
  state->stats.min_cost_steps++;
  List<Cont> alts;
  if(state->nodes_used>=f->min_cost) alts.push(A,builder().build());
  return alts;
}
