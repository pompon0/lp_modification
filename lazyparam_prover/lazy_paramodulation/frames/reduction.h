struct _ReductionFrame { Atom a; /* eq(t,s) */ };
using ReductionFrame = Variant<Frame,Frame::REDUCTION,_ReductionFrame>;
template<typename Alts> void reduction(State &state, ReductionFrame f, Alts alts) const {
  if(!state.val.unify(f->a.arg(0),f->a.arg(1))) return;
  state.lazy_clauses_used += lazy(AxiomClause{AndClause::make(f->a.neg())});
  alts(Cont{frames.tail()});
}
