struct _StrongFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  ssize_t strong_id;
};

template<typename Div> INL static void strong(Div *d, Branch branch, DerAndClause dcla, ssize_t strong_id) const { 
  d->state->stats.strong_steps++;
  auto mcla = d->state->allocate(A,f->dcla);
  if(!mcla) return;
  auto cla = mcla.get();
  STATE_FRAME(d->A,d->state,"strong(strong_id=%,cla=%)",strong_id,show(cla));
  if(f->strong_id>=0) if(!d->state->val.unify(A,branch.false_.head(),cla.atom(strong_id))) return;

  BranchSet bs{.branch = f->branch};
  for(ssize_t i=cla.atom_count(); i--;) if(i!=strong_id) {
    auto w = _WeakFrame{};
    w.branch = branch;
    w.branch.false_.push(d->A,cla.atom(i));
    branch.true_.push(d->A,cla.atom(i));
    d->and_([w](Div *d)INLL{ w.run(d); });
  }
  d->done(Features{.depth=branch.false_.size()+1}); 
}
