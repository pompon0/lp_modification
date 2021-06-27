template<typename Div> INL static void strong(Div *d, Branch branch, DerAndClause dcla, ssize_t strong_id) { 
  d->state->stats.strong_steps++;
  auto mcla = d->state->allocate(d->A,dcla);
  if(!mcla) return;
  auto cla = mcla.get();
  STATE_FRAME(d->A,d->state,"strong(strong_id=%,cla=%)",strong_id,show(cla));
  if(strong_id>=0) if(!d->state->val.unify(d->A,branch.false_.head(),cla.atom(strong_id))) return;

  d->alt([&](typename Div::Alt *x) {
    for(ssize_t i=cla.atom_count(); i--;) if(i!=strong_id) {
      auto w = _WeakFrame{};
      w.branch = branch;
      w.branch.false_.push(d->A,cla.atom(i));
      branch.true_.push(d->A,cla.atom(i));
      x->task(memory::nothing(),[w](Div *d)INLL{ w.run(d); });
    }
    x->feature_branch(branch);
  });
}
