struct _WeakSetFrame {
  size_t nodes_limit;
  size_t branch_count;
  memory::List<Branch> branches;
};

memory::List<Cont> weak_set(memory::Alloc &A, WeakSetFrame f) const { STATE_FRAME(A,state,"weak_set");
  state->stats.weak_set_steps++;
  DEBUG if(!f->branch_count) error("f->branch_count = 0");
  if(f->branch_count==1){
    auto b = WeakFrame::Builder(A);
    b->min_cost = 0;
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branches.head();
    return weak(A,b.build());
  }
  
  memory::List<Cont> alts;
  size_t per_bud = (f->nodes_limit-state->nodes_used)/f->branch_count;
  if(f->nodes_limit>state->nodes_used) { // if there is budget to allocate.
    auto wsb = WeakSetFrame::Builder(A);
    wsb->nodes_limit = f->nodes_limit-(per_bud+1);
    wsb->branch_count = f->branch_count-1;
    wsb->branches = f->branches.tail();
    auto wb = WeakFrame::Builder(A);
    wb->min_cost = per_bud+1;
    wb->nodes_limit = f->nodes_limit;
    wb->branch = f->branches.head();
    alts.push(A,builder().add(A,Frame(wb.build())).add(A,Frame(wsb.build())).build());
  }
  auto wb = WeakFrame::Builder(A);
  wb->min_cost = 0;
  wb->nodes_limit = state->nodes_used + per_bud;
  wb->branch = f->branches.head();
  auto wsb = WeakSetFrame::Builder(A);
  wsb->nodes_limit = f->nodes_limit;
  wsb->branch_count = f->branch_count-1;
  wsb->branches = f->branches.tail();
  alts.push(A,builder().add(A,Frame(wsb.build())).add(A,Frame(wb.build())).build());
  return alts;
}
