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

  template<typename Div> INL void lazy_strong_connection(Div *d) const { STATE_FRAME(A,state,"lazy_strong_connection(branch_lr=%,L=%,p=%,l=%,r=%)",base.branch_lr,show(base.L.A),show(base.L.get()),show(l),show(r));
    if(!d->state->val.push_constraint(A,OrderAtom(A,OrderAtom::G,l,r))) return;
    // -L[f(v)], L[w], f(v)=w
    auto bs = base.branch_set;
    if(!base.branch_lr) {
      if(l.type()==Term::VAR) {
        // L[p],z/=r | (p=z>w), L[w], r=w
        //   -L[p], L[w], z=w
        //   z=r, r=w z/=w
        AtomPath L = base.L;
        Term z = l;
        Term w(base.w);
        Term p = L.get();
        Term r = r;
        // unify
        if(!state->val.push_constraint(A,OrderAtom(A,OrderAtom::G,z,w))) return;
        if(!state->val.unify(A,p,z)) return memory::nothing();
        Atom r_w = Atom::eq(A,true,r,w);
        Atom z_r = Atom::eq(A,true,z,r);
        Atom z_w = Atom::eq(A,true,z,w);
        bs.push(A,L.replace(A,w));
        bs.push(A,r_w);
        d->state->lazy_clauses_used.push(A,lazy(A,ApClause(L,w)));
        d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,z_r,r_w,z_w.neg())}));
      } else {
        // L[p], f(s)/=r | (p=f(v)>w), L[w], r=w, s_i=v_i
        //    -L[p], L[w], f(v)=w
        //    f(v)=f(s), f(s)=r, f(v)/=r
        //    f(v)=r, r=w, f(v)/=w
        //    s_i=v_i, f(v)/=f(s)
        AtomPath L = base.L;
        Term p = L.get();
        Term fs = l;
        Term r = r;
        Term w(base.w);
        size_t n = Fun(fs).arg_count();
        Fun::Builder fvb(A,Fun(fs).fun(),n);
        AndClause::Builder mb(A,n+1);
        for(size_t i=n; i--;) {
          Term vi(d->state->val.allocate(Var(A,0)));
          Term si = Fun(fs).arg(i);
          Atom si_vi = Atom::eq(A,true,si,vi);
          bs.push(A,si_vi);
          mb.set_atom(i,si_vi);
          fvb.set_arg(i,vi);
        }
        Term fv(fvb.build());
        // unify
        if(!d->state->val.push_constraint(A,OrderAtom(A,OrderAtom::G,fv,w))) return;
        if(!d->state->val.unify(A,p,fv)) return;
        Atom fv_fs = Atom::eq(A,true,fv,fs);
        Atom fs_r = Atom::eq(A,true,fs,r);
        Atom fv_r = Atom::eq(A,true,fv,r);
        Atom r_w = Atom::eq(A,true,r,w);
        Atom fv_w = Atom::eq(A,true,fv,w);
        bs.push(A,L.replace(A,w));
        bs.push(A,r_w);
        mb.set_atom(n,fv_fs.neg());
        d->state->lazy_clauses_used.push(A,lazy(A,ApClause(L,w)));
        d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,fv_fs,fs_r,fv_r.neg())}));
        d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,fv_r,r_w,fv_w.neg())}));
        d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{mb.build()}));
      }
    } else {
      // l/=r, L[f(s)] |  (f(v)=l>r=w), L[w], s_i=v_i
      //    -L[f(s)], L[w], f(s)=w
      //    s_i=v_i, f(s)/=f(v)
      //    f(s)=f(v), l=r, f(s)/=w
      AtomPath L = base.L;
      Term fs = L.get();
      Term l = l;
      Term r = r;
      Term w(f->base.w);
      size_t n = Fun(fs).arg_count();
      Fun::Builder fvb(A,Fun(fs).fun(),n);
      AndClause::Builder mb(A,n+1);
      for(size_t i=n; i--;) {
        Term vi(state->val.allocate(Var(A,0)));
        Term si = Fun(fs).arg(i);
        Atom si_vi = Atom::eq(A,true,si,vi);
        bs.push(A,si_vi);
        mb.set_atom(i,si_vi);
        fvb.set_arg(i,vi);
      }
      Term fv(fvb.build());
      // unify
      if(!d->state->val.push_constraint(A,OrderAtom(A,OrderAtom::G,l,r))) return;
      if(!d->state->val.unify(A,fv,l)) return;
      if(!d->state->val.unify(A,r,w)) return;
      Atom fs_fv = Atom::eq(A,true,fs,fv);
      Atom l_r = Atom::eq(A,true,l,r);
      Atom fs_w = Atom::eq(A,true,fs,w);
      bs.push(A,L.replace(A,w));
      mb.set_atom(n,fs_fv.neg());
      d->state->lazy_clauses_used.push(A,lazy(A,ApClause(L,w)));
      d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{mb.build()}));
      d->state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,fs_fv,l_r,fs_w.neg())}));
    }
    Features f{.depth=branch.size()+1};
    for(auto brs = branches; !brs.empty(); brs = brs.tail()) {
      d->and_(f,[brs](Div *d){ _WeakFrame{.branch=brs.head()}.run(d); });
    }
    d->done(f);
  }
};
