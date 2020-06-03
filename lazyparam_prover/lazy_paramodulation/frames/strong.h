struct _StrongFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  ssize_t strong_id;
};
using StrongFrame = Variant<Frame,Frame::STRONG,_StrongFrame>;

template<typename Alts> void strong(State &state, StrongFrame f, Alts alts) const { STATE_FRAME(state,"strong(%,%, branch=%)",show(f->dcla),f->strong_id,show(f->branch));
  state.stats.strong_steps++;
  auto cla = state.allocate(f->dcla);
  if(f->strong_id>=0) if(!state.val.unify(f->branch.false_.head(),cla.atom(f->strong_id))) return;

  BranchSet bs{.branch = f->branch};
  for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) bs.push(cla.atom(i));
  if(bs.branches_size==0) { alts(Cont{frames.tail()}); return; }
  WeakSetFrame::Builder b;
  b->nodes_limit = f->nodes_limit;
  b->branches = bs.branches;
  b->branch_count = bs.branches_size; 
  alts(Cont{Frame(b.build()) + frames.tail()});
}
