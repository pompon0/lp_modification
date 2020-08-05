struct _WeakUnifyFrame { Atom a1,a2; };
using WeakUnifyFrame = Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
List<Cont> weak_unify(WeakUnifyFrame f) const { STATE_FRAME(state,"weak_unify(%,%)",show(f->a1),show(f->a2));
  state->stats.weak_unify_steps++;
  List<Cont> alts;
  if(state->val.unify(f->a1,f->a2)) alts.push(state->A,builder().build()); 
  return alts;
}

