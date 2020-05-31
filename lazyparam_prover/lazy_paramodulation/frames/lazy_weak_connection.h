struct _LazyWeakConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  Term l,r;
  Var w;
  AtomPath L;
};
using LazyWeakConnectionFrame = Variant<Frame,Frame::LAZY_WEAK_CONNECTION,_LazyWeakConnectionFrame>;

template<typename Alts> void lazy_weak_connection(State &state, LazyWeakConnectionFrame f, Alts alts) const { FRAME("lazy_weak_connection()");
  if(!state.val.unify(f->l,f->L.get())) return;
  if(!state.val.unify(f->r,Term(f->w))) error("mgu(w,r) failed");
  state.lazy_clauses_used += Lazy<DerAndClause>(alloc_init(ApClause(f->L,Term(f->w))));
  WeakFrame::Builder b;
  b->nodes_limit = f->nodes_limit;
  b->min_cost = 0; 
  b->branch = Branch{f->L.replace(Term(f->w)) + f->branch.false_, f->branch.true_};
  alts(Cont{Frame(b.build())+frames.tail()});
}
