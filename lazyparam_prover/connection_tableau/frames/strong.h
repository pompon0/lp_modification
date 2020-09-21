struct _StrongFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  ssize_t strong_id;
};
using StrongFrame = memory::Variant<Frame,Frame::STRONG,_StrongFrame>;
INL memory::List<Cont> strong(memory::Alloc &A, StrongFrame f) const { FRAME("strong(%,%)",show(f->dcla),f->strong_id);
  state->stats.strong_steps++;
  auto mcla = state->allocate(A,f->dcla);
  if(!mcla) return memory::nothing();
  auto cla = mcla.get();
  if(f->strong_id>=0) if(!state->val.unify(A,f->branch.false_.head(),cla.atom(f->strong_id))) return memory::nothing();

  memory::List<Atom> todo;
  for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) todo.push(A,cla.atom(i));
  auto matoms = strong_resolution(A,f->nodes_limit,todo);
  if(!matoms) return memory::nothing();
  
  WeakConnectionsFrame::Builder b(A);
  b->nodes_limit = f->nodes_limit;
  b->atoms = matoms.get();
  b->branches = memory::nothing();
  b->branch_count = 0;
  b->next = f->branch;
  return memory::List<Cont>(A,builder().add(A,Frame(b.build())).build());;
}

INL memory::Maybe<memory::List<Atom>> strong_resolution(memory::Alloc &A, size_t nodes_limit, memory::List<Atom> todo) const { FRAME("strong_resolution()");
  memory::List<Atom> checked;
  while(!todo.empty()) {
    auto a = todo.head();
    todo = todo.tail();
    // Look for strong only atoms.
    if(!a.strong_only()) { checked.push(A,a); continue; }
    size_t budget = nodes_limit - state->nodes_used;
    auto filter = state->cla_index.get_matches(a,budget);
    auto mca = filter.next();
    if(!mca) return memory::nothing();
    // Filter out those which have more that 1 possible unification.
    if(filter.next()) { checked.push(A,a); continue; }
    state->stats.strong_only_steps++;
    auto ca = mca.get();
    // Connect the new clause and analyze the new atoms recursively.
    auto mcla = state->allocate(A,ca.cla);
    if(!mcla) return memory::nothing();
    auto cla = mcla.get();
    if(!state->val.unify(A,a,cla.atom(ca.i))) return memory::nothing();
    for(size_t i=cla.atom_count(); i--;) if(i!=ca.i) todo.push(A,cla.atom(i));
  }
  return just(checked);
}
