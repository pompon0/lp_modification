struct _LazyPreWeakConnectionFrame {
  Branch branch;
  Atom L;
  Atom lr;

  template<typename Div> INL void lazy_pre_weak_connection(Div *d) const { STATE_FRAME(A,state,"lazy_pre_weak_connection(L=%,lr=%)",show(L),show(lr));
    if(++d->state->nodes_used>d->size_limit) return;
    auto lr = lr;
    DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
    auto l = lr.arg(0);
    auto r = lr.arg(1);
    if(!d->state->val.push_constraint(A,OrderAtom(A,OrderAtom::G,l,r))) return;
    d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,
      lr.neg(),
      Atom::eq(A,lr.sign(),r,l)
    )}));

    Features f{.depth=branch.size()+1};
    Var w = state->val.allocate(Var(A,0));
    for(auto apl = paths(A,f->L); !apl.empty(); apl = apl.tail()) {
      _LazyWeakConnectionFrame::Base base {
        .nodes_limit = f->nodes_limit,
        .branch = f->branch,
        .w = w,
        .L = apl.head(),
      };
      d->or_(f,[base,l,r](Div *d){ _LazyWeakConnectionFrame{.base = base, .l=l, .r=r}.run(d); });
      d->or_(f,[base,l,r](Div *d){ _LazyWeakConnectionFrame(.base = base, .l=r, .r=l}.run(d); });
    }
    return alts;
  }
};
