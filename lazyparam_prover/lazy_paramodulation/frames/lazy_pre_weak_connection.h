struct _LazyPreWeakConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  Atom L;
  Atom lr;
};

memory::List<Cont> lazy_pre_weak_connection(memory::Alloc &A, LazyPreWeakConnectionFrame f) const { STATE_FRAME(A,state,"lazy_pre_weak_connection(L=%,lr=%)",show(f->L),show(f->lr));
  if(++state->nodes_used>f->nodes_limit) return memory::nothing();
  auto lr = f->lr;
  DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
  auto l = lr.arg(0);
  auto r = lr.arg(1);
  if(!state->val.push_constraint(A,OrderAtom(A,OrderAtom::G,l,r))) return memory::nothing();
  state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,
    lr.neg(),
    Atom::eq(A,lr.sign(),r,l)
  )}));

  memory::List<Cont> alts;
  Var w = state->val.allocate(Var(A,0));
  for(auto apl = paths(A,f->L); !apl.empty(); apl = apl.tail()) {
    _LazyWeakConnectionFrame::Base base {
      .nodes_limit = f->nodes_limit,
      .branch = f->branch,
      .w = w,
      .L = apl.head(),
    };
    auto b1 = LazyWeakConnectionFrame::alloc(A);
    auto b2 = LazyWeakConnectionFrame::alloc(A);
    b1->base = base; b1->l = l; b1->r = r;
    b2->base = base; b2->l = r; b2->r = l;
    alts.push(A,builder().add(A,Frame(b1)).build());
    alts.push(A,builder().add(A,Frame(b2)).build());
  }
  return alts;
}
