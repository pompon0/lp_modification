struct _LazyPreWeakConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  Atom L;
  Atom lr;
};
using LazyPreWeakConnectionFrame = Variant<Frame,Frame::LAZY_PRE_WEAK_CONNECTION,_LazyPreWeakConnectionFrame>;

template<typename Alts> void lazy_pre_weak_connection(State &state, LazyPreWeakConnectionFrame f, Alts alts) const { FRAME("lazy_pre_weak_connection()");
  if(++state.nodes_used>f->nodes_limit) return;
  auto lr = f->lr;
  DEBUG if(lr.pred()!=Atom::EQ || lr.sign()) error("lr = %",show(lr));
  auto l = lr.arg(0);
  auto r = lr.arg(1);
  state.val.push_constraint(OrderAtom(OrderAtom::G,l,r));
  state.lazy_clauses_used += lazy(AxiomClause{AndClause::make(
    lr.neg(),
    Atom::eq(lr.sign(),r,l)
  )});

  Var w = state.val.allocate(Var(0));
  for(auto apl = paths(f->L); !apl.empty(); apl = apl.tail()) {
    _LazyWeakConnectionFrame::Base base {
      .nodes_limit = f->nodes_limit,
      .branch = f->branch,
      .w = w,
      .L = apl.head(),
    };
    LazyWeakConnectionFrame::Builder b1,b2;
    b1->base = base; b1->l = l; b1->r = r;
    b2->base = base; b2->l = r; b2->r = l;
    alts(Cont{Frame(b1.build())+frames.tail()});
    alts(Cont{Frame(b2.build())+frames.tail()});
  }
}
