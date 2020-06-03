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

template<typename Alts> void lazy_strong_connection(State &state, LazyStrongConnectionFrame f, Alts alts) const { STATE_FRAME(state,"lazy_strong_connection(branch_lr=%,L=%,p=%,l=%,r=%)",f->base.branch_lr,show(f->base.L.A),show(f->base.L.get()),show(f->l),show(f->r));
  state.val.push_constraint(OrderAtom(OrderAtom::G,f->l,f->r));
  // -L[f(v)], L[w], f(v)=w
  auto bs = f->base.branch_set;
  Term w(f->base.w);
  if(!f->base.branch_lr) {
    if(f->l.type()==Term::VAR) {
      // L[p],z/=r | (p=z>w), L[w], r=w
      //   -L[p], L[w], z=w
      //   z=r, r=w z/=w
      AtomPath L = f->base.L;
      Term z = f->l;
      Term w(f->base.w);
      Term p = L.get();
      Term r = f->r;
      // unify
      state.val.push_constraint(OrderAtom(OrderAtom::G,z,w));
      if(!state.val.unify(p,z)) return;
      Atom r_w = Atom::eq(true,r,w);
      Atom z_r = Atom::eq(true,z,r);
      Atom z_w = Atom::eq(true,z,w);
      bs.push(L.replace(w));
      bs.push(r_w);
      state.lazy_clauses_used += lazy(ApClause(L,w));
      state.lazy_clauses_used += lazy(AxiomClause{AndClause::make(z_r,r_w,z_w.neg())});
    } else {
      // L[p], f(s)/=r | (p=f(v)>w), L[w], r=w, s_i=v_i
      //    -L[p], L[w], f(v)=w
      //    f(v)=f(s), f(s)=r, f(v)/=r
      //    f(v)=r, r=w, f(v)/=w
      //    s_i=v_i, f(v)/=f(s)
      AtomPath L = f->base.L;
      Term p = L.get();
      Term fs = f->l;
      Term r = f->r;
      Term w(f->base.w);
      size_t n = Fun(fs).arg_count();
      Fun::Builder fvb(Fun(fs).fun(),n);
      AndClause::Builder mb(n+1);
      for(size_t i=n; i--;) {
        Term vi(state.val.allocate(Var(0)));
        Term si = Fun(fs).arg(i);
        Atom si_vi = Atom::eq(true,si,vi);
        bs.push(si_vi);
        mb.set_atom(i,si_vi);
        fvb.set_arg(i,vi);
      }
      Term fv(fvb.build());
      // unify
      state.val.push_constraint(OrderAtom(OrderAtom::G,fv,w));
      if(!state.val.unify(p,fv)) return;
      Atom fv_fs = Atom::eq(true,fv,fs);
      Atom fs_r = Atom::eq(true,fs,r);
      Atom fv_r = Atom::eq(true,fv,r);
      Atom r_w = Atom::eq(true,r,w);
      Atom fv_w = Atom::eq(true,fv,w);
      bs.push(L.replace(w));
      bs.push(r_w);
      mb.set_atom(n,fv_fs.neg());
      state.lazy_clauses_used += lazy(ApClause(L,w));
      state.lazy_clauses_used += lazy(AxiomClause{AndClause::make(fv_fs,fs_r,fv_r.neg())});
      state.lazy_clauses_used += lazy(AxiomClause{AndClause::make(fv_r,r_w,fv_w.neg())});
      state.lazy_clauses_used += lazy(AxiomClause{mb.build()});
    }
  } else {
    // l/=r, L[f(s)] |  (f(v)=l>r=w), L[w], s_i=v_i
    //    -L[f(s)], L[w], f(s)=w
    //    s_i=v_i, f(s)/=f(v)
    //    f(s)=f(v), l=r, f(s)/=w
    AtomPath L = f->base.L;
    Term fs = L.get();
    Term l = f->l;
    Term r = f->r;
    Term w(f->base.w);
    size_t n = Fun(fs).arg_count();
    Fun::Builder fvb(Fun(fs).fun(),n);
    AndClause::Builder mb(n+1);
    for(size_t i=n; i--;) {
      Term vi(state.val.allocate(Var(0)));
      Term si = Fun(fs).arg(i);
      Atom si_vi = Atom::eq(true,si,vi);
      bs.push(si_vi);
      mb.set_atom(i,si_vi);
      fvb.set_arg(i,vi);
    }
    Term fv(fvb.build());
    // unify
    state.val.push_constraint(OrderAtom(OrderAtom::G,l,r));
    if(!state.val.unify(fv,l)) return;
    if(!state.val.unify(r,w)) return;
    Atom fs_fv = Atom::eq(true,fs,fv);
    Atom l_r = Atom::eq(true,l,r);
    Atom fs_w = Atom::eq(true,fs,w);
    bs.push(L.replace(w));
    mb.set_atom(n,fs_fv.neg());
    state.lazy_clauses_used += lazy(ApClause(L,w));
    state.lazy_clauses_used += lazy(AxiomClause{mb.build()});
    state.lazy_clauses_used += lazy(AxiomClause{AndClause::make(fs_fv,l_r,fs_w.neg())});
  }
  WeakSetFrame::Builder b;
  b->nodes_limit = f->base.nodes_limit;
  b->branches = bs.branches;
  b->branch_count = bs.branches_size;
  alts(Cont{Frame(b.build()) + frames.tail()});
}
