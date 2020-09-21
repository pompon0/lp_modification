struct _ReductionFrame { Atom a; /* eq(t,s) */ };
using ReductionFrame = memory::Variant<Frame,Frame::REDUCTION,_ReductionFrame>;
memory::List<Cont> reduction(memory::Alloc &A, ReductionFrame f) const { STATE_FRAME(A,state,"reduction(%)",show(f->a));
  if(!state->val.unify(A,f->a.arg(0),f->a.arg(1))) return memory::nothing();
  state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,f->a.neg())}));
  return memory::List<Cont>(A,builder().build());
}
