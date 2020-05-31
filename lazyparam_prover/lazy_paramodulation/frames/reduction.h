struct _ReductionFrame { Atom a; };
using ReductionFrame = Variant<Frame,Frame::REDUCTION,_ReductionFrame>;
template<typename Alts> void reduction(State &state, ReductionFrame f, Alts alts) const {
  if(!state.val.unify(f->a.arg(0),f->a.arg(1))) return;
  // TODO: push reflection axiom
  //
  alts(Cont{frames.tail()});
}
