struct _LazyPreWeakConnectionFrame {
  Branch branch;
  Atom L;
  Atom lr;

  template<typename Div> INL void run(Div *d) const { STATE_FRAME(d->A,d->state,"lazy_pre_weak_connection(L=%,lr=%)",show(L),show(lr));
    if(++d->state->nodes_used>d->size_limit) return;
    DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
    auto l = lr.arg(0);
    auto r = lr.arg(1);
    if(!d->state->val.push_constraint(d->A,OrderAtom(d->A,OrderAtom::G,l,r))) return;
    d->state->lazy_clauses_used.push(d->A,lazy(d->A,AxiomClause{AndClause::make(d->A,
      lr.neg(),
      Atom::eq(d->A,lr.sign(),r,l)
    )}));

    typename Div::Features f;
    f.set_depth([&]()INLL{ return branch.false_.size()+1; });
    Var w = d->state->val.allocate(Var(d->A,0));
    for(auto apl = paths(d->A,L); !apl.empty(); apl = apl.tail()) {
      _LazyWeakConnectionFrame::Base base {
        .branch = branch,
        .w = w,
        .L = apl.head(),
      };
      d->or_(f,[base,l,r](Div *d)INLL{ _LazyWeakConnectionFrame{.base = base, .l=l, .r=r}.run(d); });
      d->or_(f,[base,l,r](Div *d)INLL{ _LazyWeakConnectionFrame{.base = base, .l=r, .r=l}.run(d); });
    }
  }
};
