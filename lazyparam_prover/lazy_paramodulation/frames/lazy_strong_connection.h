struct _LazyStrongConnectionFrame {
  struct Base {
    size_t nodes_limit;
    BranchSet branch_set;
    
    Var w;
    AtomPath L;
    bool branch_lr;
  };
  Base base;
  Term l,r;
};
using LazyStrongConnectionFrame = Variant<Frame,Frame::LAZY_STRONG_CONNECTION,_LazyStrongConnectionFrame>;

template<typename Alts> void lazy_strong_connection(State &state, LazyStrongConnectionFrame f, Alts alts) const { FRAME("lazy_strong_connection()");
  state.val.push_constraint(OrderAtom(OrderAtom::G,f->l,f->r));
  state.lazy_clauses_used += Lazy<DerAndClause>(alloc_init(ApClause(f->base.L,Term(f->base.w))));
  auto bs = f->base.branch_set;
  auto p = f->base.L.get();
  Term w(f->base.w);
  bs.push(f->base.L.replace(w)); // -L[p],L[w],p=w
  if(!f->base.branch_lr) {
    if(f->l.type()==Term::VAR) {
      // L[p], z/=r
      // (p > w)
      state.val.push_constraint(OrderAtom(OrderAtom::G,p,Term(f->base.w)));
      // r = w
      Atom rw = Atom::eq(true,f->r,w);
      bs.push(rw);
      // (p = z)
      if(!state.val.unify(p,f->l)) return;
      // z=r r=w z/=w
      // TODO: make it a lazy DerAndClause.
      // AndClause{Atom::eq(true,f->l,f->r),rw,Atom::eq(false,f->l,w)};
    } else {
      // L[p], f(s)/=r
      // (p > w)
      state.val.push_constraint(OrderAtom(OrderAtom::G,p,Term(f->base.w)));
      // r = w
      bs.push(Atom::eq(true,f->r,Term(f->base.w)));
      Fun l(f->l);
      Fun::Builder fvb(l.fun(),l.arg_count());
      for(size_t i=l.arg_count(); i--;) {
        auto v = state.val.allocate(Var(0));
        // s_i = v_i
        bs.push(Atom::eq(true,l.arg(i),Term(v)));
        fvb.set_arg(i,Term(v));
      }
      // (p = f(v))
      if(!state.val.unify(p,Term(fvb.build()))) return;
      // TODO
      // v_i = s_i f(v) /= f(s)
      // f(v)/=w f(v) = f(s) f(s) = r r = w
    }
  } else {
    // l/=r, L[f(s)]
    // (l > r)
    state.val.push_constraint(OrderAtom(OrderAtom::G,f->l,f->r));
    // (r = w)
    Fun pf(p);
    if(!state.val.unify(f->r,Term(f->base.w))) return;
    Fun::Builder fvb(pf.fun(),pf.arg_count());
    for(size_t i=pf.arg_count(); i--;) {
      auto v = state.val.allocate(Var(0));
      // s_i = v_i
      bs.push(Atom::eq(true,pf.arg(i),Term(v)));
      fvb.set_arg(i,Term(v));
    }
    // (f(v) = l)
    if(!state.val.unify(Term(fvb.build()),f->l)) return;
    // TODO:
    // s_i = v_i f(s) /= f(v)
    // f(s) /= w f(s) = f(v) f(v) = w
  }
  WeakSetFrame::Builder b;
  b->nodes_limit = f->base.nodes_limit;
  b->branches = bs.branches;
  b->branch_count = bs.branches_size;
  alts(Cont{Frame(b.build()) + frames.tail()});
}
