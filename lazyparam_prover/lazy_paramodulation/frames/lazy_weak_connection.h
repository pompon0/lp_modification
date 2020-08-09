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

List<Cont> lazy_weak_connection(memory::Alloc &A, LazyWeakConnectionFrame f) const { STATE_FRAME(A,state,"lazy_weak_connection()");
  if(!state->val.unify(A,f->l,f->base.L.get())) return nothing();
  if(!state->val.unify(A,f->r,Term(f->base.w))) error("mgu(w,r) failed");
  state->lazy_clauses_used.push(A,Lazy<DerAndClause>(A.alloc_init(ApClause(f->base.L,Term(f->base.w)))));
  WeakFrame::Builder b(A);
  b->nodes_limit = f->base.nodes_limit;
  b->min_cost = 0; 
  b->branch = Branch{
    .false_ = f->base.branch.false_.add(A,f->base.L.replace(A,Term(f->base.w))),
    .true_ = f->base.branch.true_,
  };
  return List<Cont>(A,builder().add(A,Frame(b.build())).build());
}
