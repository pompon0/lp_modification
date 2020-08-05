struct _StrongFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  ssize_t strong_id;
};
using StrongFrame = Variant<Frame,Frame::STRONG,_StrongFrame>;

List<Cont> strong(StrongFrame f) const { 
  state->stats.strong_steps++;
  auto mcla = state->allocate(f->dcla);
  if(!mcla) return nothing();
  auto cla = mcla.get();
  STATE_FRAME(state,"strong(strong_id=%,cla=%)",f->strong_id,show(cla));
  if(f->strong_id>=0) if(!state->val.unify(f->branch.false_.head(),cla.atom(f->strong_id))) return nothing();

  List<Cont> alts;
  BranchSet bs{.branch = f->branch};
  for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) bs.push(state->A,cla.atom(i));
  if(bs.branches_size==0) { alts.push(state->A,builder().build()); return alts; }
  WeakSetFrame::Builder b(state->A);
  b->nodes_limit = f->nodes_limit;
  b->branches = bs.branches;
  b->branch_count = bs.branches_size; 
  alts.push(state->A,builder().add(Frame(b.build())).build());
  return alts;
}
