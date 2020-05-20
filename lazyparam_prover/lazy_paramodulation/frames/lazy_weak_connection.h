struct _LazyWeakConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  ssize_t strong_id;
};
using StrongFrame = Variant<Frame,Frame::STRONG,_StrongFrame>;

template<typename Alts> void strong(State &state, StrongFrame f, Alts alts) const { FRAME("strong(%,%)",show(f->dcla),f->strong_id);
  state.stats.strong_steps++;
  auto cla = state.allocate(f->dcla);
  if(f->strong_id>=0) if(!state.val.unify(f->branch.false_.head(),cla.atom(f->strong_id))) return;

  List<Atom> todo;
  for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) todo += cla.atom(i);
  
  WeakConnectionsFrame::Builder b;
  b->nodes_limit = f->nodes_limit;
  b->atoms = todo;
  b->branches = List<Branch>();
  b->branch_count = 0;
  b->next = f->branch;
  alts(Cont{Frame(b.build()) + frames.tail()});
}
