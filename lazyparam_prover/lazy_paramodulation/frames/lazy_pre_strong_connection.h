struct _LazyPreStrongConnectionFrame {
  Branch branch;
  DerAndClause dcla;
  size_t strong_id;
  bool branch_lr;

  template<typename Div> INL void run(Div *d) const { 
    auto mcla = d->state->allocate(d->A,dcla);
    if(!mcla) return;
    auto cla = mcla.get();
    STATE_FRAME(d->A,d->state,"lazy_pre_strong_connection(strong_id=%,cla=%)",strong_id,show(cla));
    if(dcla.cost()==0) d->state->nodes_used++; // this implementation doesn't check regularity, so it doesn't have halting property with free clauses.
    if(d->state->nodes_used>d->size_limit()) return;

    BranchSet bs{.branch = branch};
    for(size_t i=cla.atom_count(); i--;) if(i!=strong_id) bs.push(d->A,cla.atom(i));

    auto L = branch.false_.head();
    auto lr = cla.atom(strong_id);
    if(branch_lr) std::swap(L,lr);
    DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
    auto l = lr.arg(0);
    auto r = lr.arg(1);
    d->state->lazy_clauses_used.push(d->A,lazy(d->A,AxiomClause{AndClause::make(d->A,
      lr.neg(),
      Atom::eq(d->A,lr.sign(),r,l)
    )}));

    Var w = d->state->val.allocate(Var(d->A,0));
    for(auto apl = paths(d->A,L); !apl.empty(); apl = apl.tail()) {
      _LazyStrongConnectionFrame::Base base {
        .branch_set = bs,
        .w = w,
        .L = apl.head(),
        .branch_lr = branch_lr,
      };
      d->alt([&](typename Div::Alt *x)INLL{
        x->feature_branch(branch);
        x->task(memory::nothing(),[base,l,r](Div *d)INLL{
          _LazyStrongConnectionFrame{.base=base, .l=l, .r=r}.run(d);
        });
      });
      d->alt([&](typename Div::Alt *x)INLL{
        x->feature_branch(branch);
        x->task(memory::nothing(),[base,l,r](Div *d)INLL{
          _LazyStrongConnectionFrame{.base=base, .l=r, .r=l}.run(d);
        });
      });
    }
  }
};
