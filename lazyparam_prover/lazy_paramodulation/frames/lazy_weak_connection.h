struct _LazyWeakConnectionFrame {
  struct Base {
    Branch branch;
    Var w;
    AtomPath L;
  };
  Base base;
  Term l,r;

  template<typename Div> INL void lazy_weak_connection(Div *d) const { STATE_FRAME(d->A,d->state,"lazy_weak_connection()");
    if(!state->val.unify(d->A,l,base.L.get())) return;
    if(!state->val.unify(d->A,r,Term(base.w))) error("mgu(w,r) failed");
    d->state->lazy_clauses_used.push(d->A,memory::Lazy<DerAndClause>(d->A.alloc_init(ApClause(f->base.L,Term(f->base.w)))));
    _WeakFrame{ .branch=Branch{
      .false_ = f->base.branch.false_.add(A,f->base.L.replace(A,Term(f->base.w))),
      .true_ = f->base.branch.true_,
    }}.run(d);
  }
};
