#ifndef CONNECTION_TABLEAU_FRAMES_STRONG_H_
#define CONNECTION_TABLEAU_FRAMES_STRONG_H_

INL static memory::Maybe<TaskSet> strong(
    memory::Alloc &A, SearchState *state, Branch branch, DerAndClause dcla, ssize_t strong_id) {
  STATE_FRAME(A,state,"strong(%,%)",show(dcla),strong_id);
  state->stats.strong_steps++;
  auto mcla = state->allocate(A,dcla);
  if(!mcla) return memory::nothing();
  auto cla = mcla.get();
  if(strong_id>=0) if(!state->val.unify(A,branch.false_.head(),cla.atom(strong_id))) return memory::nothing();

  memory::List<Atom> todo;
  for(ssize_t i=cla.atom_count(); i--;) if(i!=strong_id) todo.push(A,cla.atom(i));
  auto matoms = strong_resolution(A,state,todo);
  if(!matoms) return memory::nothing();
 
  TaskSet ts;
  for(auto atoms = matoms.get(); !atoms.empty(); atoms = atoms.tail()) {
    auto b = WeakFrame::Builder(A);
    b->branch = branch;
    b->branch.false_.push(A,atoms.head());
    branch.true_.push(A,atoms.head());
    ts.push(A,Task(b.build()));
  }
  STATE_LOG(A,state,"ts.size() = %",ts.size());
  return just(ts);
}

INL static memory::Maybe<memory::List<Atom>> strong_resolution(memory::Alloc &A, SearchState *state, memory::List<Atom> todo) { FRAME("strong_resolution()");
  memory::List<Atom> checked;
  while(!todo.empty()) {
    auto a = todo.head();
    todo = todo.tail();
    // Look for strong only atoms.
    if(!a.strong_only()) { checked.push(A,a); continue; }
    auto filter = state->cla_index.get_matches(a,memory::nothing());
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
