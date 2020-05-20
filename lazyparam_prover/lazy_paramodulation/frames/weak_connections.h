struct _WeakConnectionsFrame {
  size_t nodes_limit;
  List<Atom> atoms;
  List<Branch> branches;
  size_t branch_count;
  Branch next;
};
using WeakConnectionsFrame = Variant<Frame,Frame::WEAK_CONNECTIONS,_WeakConnectionsFrame>;
template<typename Alts> void weak_connections(State &state, WeakConnectionsFrame f, Alts alts) const { FRAME("weak_connections");
  state.stats.weak_connections_steps++;
  if(!f->atoms.empty()) {
    Atom a = f->atoms.head(); 

    // try to do weak connection for each atom of the clause, or add a != constraint.
    auto atom_hash = Index::atom_hash(a);
    WeakConnectionsFrame::Builder cb;
    cb->nodes_limit = f->nodes_limit;
    cb->atoms = f->atoms.tail();
    cb->branches = f->branches;
    cb->branch_count = f->branch_count;
    cb->next = f->next;
    auto tail = Frame(cb.build())+frames.tail();
    // try to match with lemma
    for(auto b = f->next.true_; !b.empty(); b = b.tail()) {
      if(atom_hash!=Index::atom_hash(b.head())) continue;
      if(!state.val.equal_mod_sign(a,b.head())) continue;
      WeakConnectionsFrame::Builder cb;
      *cb = *f;
      cb->atoms = f->atoms.tail();
      alts(Cont{Frame(cb.build())+frames.tail()});
      return;
    }
    // select weak connection
    // -P(r), ..., a = P(s)
    if(a.pred()!=Atom::EQ) {
      for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        WeakUnifyFrame::Builder ub;
        ub->a1 = a;
        ub->a2 = b.head();
        alts(Cont{Frame(ub.build())+tail});
      }
    }
    // L[p],...,a = [l/=r]
    if(!a.sign() && a.pred()==Atom::EQ) {
      //TODO: move the allocation to a separate frame to minimize amount of variables.
      Var w = state.val.allocate(Var(0));
      for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
        //auto ap = split(b.head(),w);
        // Select l > r or r < l
        // unify l with p
        // allocate w and valuate it to r
        // add L[w] to atoms
      }
    }
    // l/=r,...,a = L[p]
    // TODO

    // assume that <a> doesn't occur in the path or lemmas
    {
      // add constraints (wrt path)
      for(auto b = f->next.false_; !b.empty(); b = b.tail())
        if(!state.val.push_constraint(OrderAtom::neq(a,b.head()))) return;
      WeakConnectionsFrame::Builder cb;
      cb->nodes_limit = f->nodes_limit;
      cb->atoms = f->atoms.tail();
      cb->branches = Branch{a + f->next.false_, f->next.true_} + f->branches;
      cb->branch_count = f->branch_count + 1;
      cb->next = Branch { f->next.false_, a + f->next.true_};
      alts(Cont{Frame(cb.build())+frames.tail()});
    }
  } else if(f->branch_count) {
    WeakSetFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branches = f->branches;
    b->branch_count = f->branch_count;
    alts(Cont{Frame(b.build()) + frames.tail()});
  } else {
    alts(Cont{frames.tail()});
  }
}
