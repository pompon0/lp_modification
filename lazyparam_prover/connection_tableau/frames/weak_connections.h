struct _WeakConnectionsFrame {
  size_t nodes_limit;
  List<Atom> atoms;
  List<Branch> branches;
  size_t branch_count;
  Branch next;
};
using WeakConnectionsFrame = Variant<Frame,Frame::WEAK_CONNECTIONS,_WeakConnectionsFrame>;
void weak_connections(WeakConnectionsFrame f) const { FRAME("weak_connections");
  state->stats.weak_connections_steps++;
  List<Cont> alts;
  if(!f->atoms.empty()) {
    Atom a = f->atoms.head(); 

    // try to do weak connection for each atom of the clause, or add a != constraint.
    // TODO: add constraints preventing clauses becoming contradictory
    auto atom_hash = Index::atom_hash(a);
    WeakConnectionsFrame::Builder cb;
    cb->nodes_limit = f->nodes_limit;
    cb->atoms = f->atoms.tail();
    cb->branches = f->branches;
    cb->branch_count = f->branch_count;
    cb->next = f->next;
    auto tail = builder().add(Frame(cb.build()));
    // try to match with lemma
    // TODO: lazy lemma - keep track of constraints which will allow you to close a branch early
    for(auto b = f->next.true_; !b.empty(); b = b.tail()) {
      if(atom_hash!=Index::atom_hash(b.head())) continue;
      if(!state->val.equal_mod_sign(a,b.head())) continue;
      alts += tail.build();
      return alts;
    }
    // try to unify with path
    if(!a.strong_only()) {
      for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        WeakUnifyFrame::Builder ub;
        ub->a1 = a;
        ub->a2 = b.head();
        alts += tail.add(Frame(ub.build()).build());
      }
    }
    
    // assume that <a> doesn't occur in the path or lemmas
    {
      // add constraints (wrt path)
      if(!a.strong_only()) {
        for(auto b = f->next.false_; !b.empty(); b = b.tail())
          if(!state->val.push_constraint(OrderAtom::neq(a,b.head()))) return;
      }
      WeakConnectionsFrame::Builder cb;
      cb->nodes_limit = f->nodes_limit;
      cb->atoms = f->atoms.tail();
      cb->branches = Branch{a + f->next.false_, f->next.true_} + f->branches;
      cb->branch_count = f->branch_count + 1;
      cb->next = Branch { f->next.false_, a + f->next.true_};
      alts += builder().add(Frame(cb.build())).build();
    }
  } else if(f->branch_count) {
    WeakSetFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branches = f->branches;
    b->branch_count = f->branch_count;
    alts += builder().add(Frame(b.build())).build();
  } else {
    alts += builder().build();
  }
  return alts;
}
