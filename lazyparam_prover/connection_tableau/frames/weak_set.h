struct _WeakSetFrame {
  size_t nodes_limit;
  size_t branch_count;
  List<Branch> branches;
};
using WeakSetFrame = Variant<Frame,Frame::WEAK_SET,_WeakSetFrame>;

List<Cont> weak_set(WeakSetFrame f) const { FRAME("weak_set");
  state.stats.weak_set_steps++;
  DEBUG if(!f->branch_count) error("f->branch_count = 0");
  if(f->branch_count==1){
    WeakFrame::Builder b;
    b->min_cost = 0;
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branches.head();
    return weak(b.build());
  }
 
  List<Cont> alts;
  size_t per_bud = (f->nodes_limit-state.nodes_used)/f->branch_count;
  if(f->nodes_limit>state->nodes_used) { // if there is budget to allocate.
    WeakSetFrame::Builder wsb;
    wsb->nodes_limit = f->nodes_limit-(per_bud+1);
    wsb->branch_count = f->branch_count-1;
    wsb->branches = f->branches.tail();
    WeakFrame::Builder wb;
    wb->min_cost = per_bud+1;
    wb->nodes_limit = f->nodes_limit;
    wb->branch = f->branches.head();
    alts += builder().add(Frame(wb.build())).add(Frame(wsb.build())).build();
  }
  WeakFrame::Builder wb;
  wb->min_cost = 0;
  wb->nodes_limit = state.nodes_used + per_bud;
  wb->branch = f->branches.head();
  WeakSetFrame::Builder wsb;
  wsb->nodes_limit = f->nodes_limit;
  wsb->branch_count = f->branch_count-1;
  wsb->branches = f->branches.tail();
  alts += builder().add(Frame(wsb.build())).add(Frame(wb.build())).build();
  return alts;
}

