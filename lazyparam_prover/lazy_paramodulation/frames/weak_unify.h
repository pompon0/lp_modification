struct _WeakUnifyFrame { Atom a1,a2; };
using WeakUnifyFrame = memory::Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
memory::List<Cont> weak_unify(memory::Alloc &A, WeakUnifyFrame f) const { STATE_FRAME(A,state,"weak_unify(%,%)",show(f->a1),show(f->a2));
  state->stats.weak_unify_steps++;
  memory::List<Cont> alts;
  if(state->val.unify(A,f->a1,f->a2)) alts.push(A,builder().build()); 
  return alts;
}

