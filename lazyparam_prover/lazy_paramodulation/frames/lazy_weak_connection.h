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
using LazyWeakConnectionFrame = Variant<Frame,Frame::LAZY_WEAK_CONNECTION,_LazyWeakConnectionFrame>;

List<Cont> lazy_weak_connection(LazyWeakConnectionFrame f) const { STATE_FRAME(state,"lazy_weak_connection()");
  if(!state->val.unify(f->l,f->base.L.get())) return nothing();
  if(!state->val.unify(f->r,Term(f->base.w))) error("mgu(w,r) failed");
  state->lazy_clauses_used.push(state->A,Lazy<DerAndClause>(state->A.alloc_init(ApClause(f->base.L,Term(f->base.w)))));
  WeakFrame::Builder b(state->A);
  b->nodes_limit = f->base.nodes_limit;
  b->min_cost = 0; 
  b->branch = Branch{
    .false_ = f->base.branch.false_.add(state->A,f->base.L.replace(state->A,Term(f->base.w))),
    .true_ = f->base.branch.true_,
  };
  return List<Cont>(state->A,builder().add(Frame(b.build())).build());
}
