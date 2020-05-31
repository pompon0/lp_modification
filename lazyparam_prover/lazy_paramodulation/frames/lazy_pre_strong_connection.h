struct _LazyPreStrongConnectionFrame {
  size_t nodes_limit;
  Branch branch;
  DerAndClause dcla;
  size_t strong_id;
  bool branch_lr;
};
using LazyPreStrongConnectionFrame = Variant<Frame,Frame::LAZY_PRE_STRONG_CONNECTION,_LazyPreStrongConnectionFrame>;

template<typename Alts> void lazy_pre_strong_connection(State &state, LazyPreStrongConnectionFrame f, Alts alts) const { FRAME("lazy_pre_weak_connection()");
  auto cla = state.allocate(f->dcla);
  if(f->dcla.cost()==0) state.nodes_used++; // this implementation doesn't check regularity, so it doesn't have halting property with free clauses .
  if(state.nodes_used>f->nodes_limit) return;

  BranchSet bs{.branch = f->branch};
  for(size_t i=cla.atom_count(); i--;) if(i!=f->strong_id) bs.push(cla.atom(i));

  auto L = f->branch.false_.head();
  auto lr = cla.atom(f->strong_id);
  if(f->branch_lr) std::swap(L,lr);
  DEBUG if(lr.pred()!=Atom::EQ) error("lr = %",show(lr));
  auto l = lr.arg(0);
  auto r = lr.arg(1);

  Var w = state.val.allocate(Var(0));
  for(auto apl = paths(L); !apl.empty(); apl = apl.tail()) {
    _LazyStrongConnectionFrame::Base base {
      .nodes_limit = f->nodes_limit,
      .branch_set = bs,
      .w = w,
      .L = apl.head(),
      .branch_lr = f->branch_lr,
    };
    LazyStrongConnectionFrame::Builder b1,b2;
    b1->base = base; b1->l = l; b1->r = r;
    b2->base = base; b2->l = r; b2->r = l;
    alts(Cont{Frame(b1.build())+frames.tail()});
    alts(Cont{Frame(b2.build())+frames.tail()});
  }
}
