struct _ReductionFrame { Atom a; /* eq(t,s) */ };
using ReductionFrame = Variant<Frame,Frame::REDUCTION,_ReductionFrame>;
List<Cont> reduction(ReductionFrame f) const { STATE_FRAME(state,"reduction(%)",show(f->a));
  if(!state->val.unify(f->a.arg(0),f->a.arg(1))) return nothing();
  state->lazy_clauses_used.push(state->A,lazy(state->A,AxiomClause{AndClause::make(state->A,f->a.neg())}));
  return List<Cont>(state->A,builder().build());
}
