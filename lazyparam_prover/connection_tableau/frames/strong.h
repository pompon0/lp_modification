#ifndef CONNECTION_TABLEAU_FRAMES_STRONG_H_
#define CONNECTION_TABLEAU_FRAMES_STRONG_H_

template<typename DState> INL static void strong(DState *d, Branch branch, DerAndClause dcla, ssize_t strong_id) {
  STATE_FRAME(d->A,d->state,"strong(%,%)",show(dcla),strong_id);
  d->state->stats.strong_steps++;
  auto mcla = d->state->allocate(d->A,dcla);
  if(!mcla) return; 
  auto cla = mcla.get();
  if(strong_id>=0) if(!d->state->val.unify(d->A,branch.false_.head(),cla.atom(strong_id))) return;

  memory::List<Atom> todo;
  for(ssize_t i=cla.atom_count(); i--;) if(i!=strong_id) todo.push(d->A,cla.atom(i));
  auto matoms = strong_resolution(d->A,d->state,todo,d->size_limit);
  if(!matoms) return;

  for(auto atoms = matoms.get(); !atoms.empty(); atoms = atoms.tail()) {
    auto w = _WeakFrame{};
    w.branch = branch;
    w.branch.false_.push(d->A,atoms.head());
    branch.true_.push(d->A,atoms.head());
    d->and_([w](DState *d)INLL{ w.run(d); });
  }
  d->done(Features{.depth=branch.false_.size()+1});
}

INL static memory::Maybe<memory::List<Atom>> strong_resolution(memory::Alloc &A, SearchState *state, memory::List<Atom> todo, size_t size_limit) { FRAME("strong_resolution()");
  memory::List<Atom> checked;
  while(!todo.empty()) {
    auto a = todo.head();
    todo = todo.tail();
    // Look for strong only atoms.
    if(!a.strong_only()) { checked.push(A,a); continue; }
    auto filter = state->cla_index.get_matches(a,memory::just(size_limit-state->nodes_used));
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

#endif  // CONNECTION_TABLEAU_FRAMES_STRONG_H_
