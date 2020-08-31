struct _WeakUnifyFrame { Atom a1,a2; };
using WeakUnifyFrame = Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
INL List<Cont> weak_unify(memory::Alloc &A, WeakUnifyFrame f) const { FRAME("weak_unify");
  state->stats.weak_unify_steps++;
  List<Cont> alts;
  if(state->val.unify(A,f->a1,f->a2)) alts.push(A,builder().build());
  return alts;
}
