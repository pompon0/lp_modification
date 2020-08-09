struct _ReductionFrame { Atom a; /* eq(t,s) */ };
using ReductionFrame = Variant<Frame,Frame::REDUCTION,_ReductionFrame>;
List<Cont> reduction(memory::Alloc &A, ReductionFrame f) const { STATE_FRAME(A,state,"reduction(%)",show(f->a));
  if(!state->val.unify(A,f->a.arg(0),f->a.arg(1))) return nothing();
  state->lazy_clauses_used.push(A,lazy(A,AxiomClause{AndClause::make(A,f->a.neg())}));
  return List<Cont>(A,builder().build());
}
