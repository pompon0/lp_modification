struct _LazyWeakConnectionFrame {
  struct Base {
    Branch branch;
    Var w;
    AtomPath L;
  };
  Base base;
  Term l,r;

  template<typename Div> INL void run(Div *d) const { STATE_FRAME(d->A,d->state,"lazy_weak_connection()");
    if(!d->state->val.unify(d->A,l,base.L.get())) return;
    if(!d->state->val.unify(d->A,r,Term(base.w))) error("mgu(w,r) failed");
    d->state->lazy_clauses_used.push(d->A,memory::Lazy<DerAndClause>(d->A.alloc_init(ApClause(base.L,Term(base.w)))));
    _WeakFrame{ .branch=Branch{
      .false_ = base.branch.false_.add(d->A,base.L.replace(d->A,Term(base.w))),
      .true_ = base.branch.true_,
    }}.run(d);
  }
};
