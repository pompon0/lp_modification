struct _WeakUnifyFrame { Atom a1,a2; };
using WeakUnifyFrame = Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
template<typename Alts> void weak_unify(State &state, WeakUnifyFrame f, Alts &alts) const { STATE_FRAME(state,"weak_unify(%,%)",show(f->a1),show(f->a2));
  state.stats.weak_unify_steps++;
  if(state.val.unify(f->a1,f->a2)) alts(Cont{frames.tail()}); 
}

