struct _StrongFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  ssize_t strong_id;
};

memory::List<Cont> strong(memory::Alloc &A, StrongFrame f) const { 
  state->stats.strong_steps++;
  auto mcla = state->allocate(A,f->dcla);
  if(!mcla) return memory::nothing();
  auto cla = mcla.get();
  STATE_FRAME(A,state,"strong(strong_id=%,cla=%)",f->strong_id,show(cla));
  if(f->strong_id>=0) if(!state->val.unify(A,f->branch.false_.head(),cla.atom(f->strong_id))) return memory::nothing();

  memory::List<Cont> alts;
  BranchSet bs{.branch = f->branch};
  for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) bs.push(A,cla.atom(i));
  if(bs.branches_size==0) { alts.push(A,builder().build()); return alts; }
  auto b = WeakSetFrame::Builder(A);
  b->nodes_limit = f->nodes_limit;
  b->branches = bs.branches;
  b->branch_count = bs.branches_size; 
  alts.push(A,builder().add(A,Frame(b.build())).build());
  return alts;
}
