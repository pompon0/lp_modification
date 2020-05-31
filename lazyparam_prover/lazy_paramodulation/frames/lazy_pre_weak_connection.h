struct _LazyPreWeakConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  Atom L;
  Term l,r;
};
using LazyPreWeakConnectionFrame = Variant<Frame,Frame::LAZY_PRE_WEAK_CONNECTION,_LazyPreWeakConnectionFrame>;

template<typename Alts> void lazy_pre_weak_connection(State &state, LazyPreWeakConnectionFrame f, Alts alts) const { FRAME("lazy_pre_weak_connection()");
  if(++state.nodes_used>f->nodes_limit) return;
  state.val.push_constraint(OrderAtom(OrderAtom::G,f->l,f->r));
  Var w = state.val.allocate(Var(0));
  for(auto apl = paths(f->L); !apl.empty(); apl = apl.tail()) {
    LazyWeakConnectionFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branch = f->branch;
    b->l = f->l;
    b->r = f->r;
    b->w = w;
    b->L = apl.head();
    alts(Cont{Frame(b.build())+frames.tail()});
  }
}
