struct _LazyPreStrongConnectionFrame {
  Branch branch;
  DerAndClause dcla;
  size_t strong_id;
  bool branch_lr;

  template<typename Div> INL void run(Div *d) const { 
    auto mcla = d->state->allocate(d->A,dcla);
    if(!mcla) return;
    auto cla = mcla.get();
    STATE_FRAME(A,state,"lazy_pre_strong_connection(strong_id=%,cla=%)",f->strong_id,show(cla));
    if(dcla.cost()==0) state->nodes_used++; // this implementation doesn't check regularity, so it doesn't have halting property with free clauses.
    if(d->state->nodes_used>f->size_limit) return;

    BranchSet bs{.branch = f->branch};
    for(size_t i=cla.atom_count(); i--;) if(i!=f->strong_id) bs.push(A,cla.atom(i));

    auto L = branch.false_.head();
    auto lr = cla.atom(f->strong_id);
    if(branch_lr) std::swap(L,lr);
    DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
    auto l = lr.arg(0);
    auto r = lr.arg(1);
    state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,
      lr.neg(),
      Atom::eq(A,lr.sign(),r,l)
    )}));

    Var w = state->val.allocate(Var(A,0));
    for(auto apl = paths(A,L); !apl.empty(); apl = apl.tail()) {
      _LazyStrongConnectionFrame::Base base {
        .nodes_limit = f->nodes_limit,
        .branch_set = bs,
        .w = w,
        .L = apl.head(),
        .branch_lr = f->branch_lr,
      };
      d->or_(f,[base,l,r](Div *d){
        _LazyStrongConnectionFrame{.base=base, .l=l, .r=r}.run(d);
      });
      d->or_(f,[base,l,r](Div *d){
        _LazyStrongConnectionFrame{.base=base, .l=r, .r=l}.run(d);
      });
    }
  }
};
