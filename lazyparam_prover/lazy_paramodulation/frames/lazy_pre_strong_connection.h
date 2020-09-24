struct _LazyPreStrongConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  size_t strong_id;
  bool branch_lr;
};

memory::List<Cont> lazy_pre_strong_connection(memory::Alloc &A, LazyPreStrongConnectionFrame f) const { 
  auto mcla = state->allocate(A,f->dcla);
  if(!mcla) return memory::nothing();
  auto cla = mcla.get();
  STATE_FRAME(A,state,"lazy_pre_strong_connection(strong_id=%,cla=%)",f->strong_id,show(cla));
  if(f->dcla.cost()==0) state->nodes_used++; // this implementation doesn't check regularity, so it doesn't have halting property with free clauses .
  if(state->nodes_used>f->nodes_limit) return memory::nothing();

  BranchSet bs{.branch = f->branch};
  for(size_t i=cla.atom_count(); i--;) if(i!=f->strong_id) bs.push(A,cla.atom(i));

  auto L = f->branch.false_.head();
  auto lr = cla.atom(f->strong_id);
  if(f->branch_lr) std::swap(L,lr);
  DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
  auto l = lr.arg(0);
  auto r = lr.arg(1);
  state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,
    lr.neg(),
    Atom::eq(A,lr.sign(),r,l)
  )}));

  memory::List<Cont> alts;
  Var w = state->val.allocate(Var(A,0));
  for(auto apl = paths(A,L); !apl.empty(); apl = apl.tail()) {
    _LazyStrongConnectionFrame::Base base {
      .nodes_limit = f->nodes_limit,
      .branch_set = bs,
      .w = w,
      .L = apl.head(),
      .branch_lr = f->branch_lr,
    };
    auto b1 = LazyStrongConnectionFrame::alloc(A);
    auto b2 = LazyStrongConnectionFrame::alloc(A);
    b1->base = base; b1->l = l; b1->r = r;
    b2->base = base; b2->l = r; b2->r = l;
    alts.push(A,builder().add(A,Frame(b1)).build());
    alts.push(A,builder().add(A,Frame(b2)).build());
  }
  return alts;
}
