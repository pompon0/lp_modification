struct _LazyWeakConnectionFrame {
  struct Base {
    size_t nodes_limit;
    Branch branch;
    Var w;
    AtomPath L;
  };
  Base base;
  Term l,r;
};

memory::List<Cont> lazy_weak_connection(memory::Alloc &A, LazyWeakConnectionFrame f) const { STATE_FRAME(A,state,"lazy_weak_connection()");
  if(!state->val.unify(A,f->l,f->base.L.get())) return memory::nothing();
  if(!state->val.unify(A,f->r,Term(f->base.w))) error("mgu(w,r) failed");
  state->lazy_clauses_used.push(A,memory::Lazy<DerAndClause>(A.alloc_init(ApClause(f->base.L,Term(f->base.w)))));
  auto b = WeakFrame::alloc(A);
  b->nodes_limit = f->base.nodes_limit;
  b->min_cost = 0; 
  b->branch = Branch{
    .false_ = f->base.branch.false_.add(A,f->base.L.replace(A,Term(f->base.w))),
    .true_ = f->base.branch.true_,
  };
  return memory::List<Cont>(A,builder().add(A,Frame(b)).build());
}
