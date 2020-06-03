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

template<typename Alts> void lazy_weak_connection(State &state, LazyWeakConnectionFrame f, Alts alts) const { FRAME("lazy_weak_connection()");
  if(!state.val.unify(f->l,f->base.L.get())) return;
  if(!state.val.unify(f->r,Term(f->base.w))) error("mgu(w,r) failed");
  state.lazy_clauses_used += Lazy<DerAndClause>(alloc_init(ApClause(f->base.L,Term(f->base.w))));
  WeakFrame::Builder b;
  b->nodes_limit = f->base.nodes_limit;
  b->min_cost = 0; 
  b->branch = Branch{f->base.L.replace(Term(f->base.w)) + f->base.branch.false_, f->base.branch.true_};
  alts(Cont{Frame(b.build())+frames.tail()});
}
