struct _WeakConnectionsFrame {
  size_t nodes_limit;
  memory::List<Atom> atoms;
  memory::List<Branch> branches;
  size_t branch_count;
  Branch next;
};
using WeakConnectionsFrame = memory::Variant<Frame,Frame::WEAK_CONNECTIONS,_WeakConnectionsFrame>;
INL memory::List<Cont> weak_connections(memory::Alloc &A, WeakConnectionsFrame f) const { FRAME("weak_connections :: len(atoms)==% branch_count==%",f->atoms.size(),f->branch_count);
  state->stats.weak_connections_steps++;
  memory::List<Cont> alts;
  if(!f->atoms.empty()) {
    Atom a = f->atoms.head(); 

    // try to do weak connection for each atom of the clause, or add a != constraint.
    // TODO: add constraints preventing clauses becoming contradictory
    auto atom_hash = Index::atom_hash(a);
    WeakConnectionsFrame::Builder cb(A);
    cb->nodes_limit = f->nodes_limit;
    cb->atoms = f->atoms.tail();
    cb->branches = f->branches;
    cb->branch_count = f->branch_count;
    cb->next = f->next;
    auto tail = builder().add(A,Frame(cb.build()));
    // try to match with lemma
    // TODO: lazy lemma - keep track of constraints which will allow you to close a branch early
    for(auto b = f->next.true_; !b.empty(); b = b.tail()) {
      if(atom_hash!=Index::atom_hash(b.head())) continue;
      if(!state->val.equal_mod_sign(a,b.head())) continue;
      alts.push(A,tail.build());
      return alts;
    }
    // try to unify with path
    if(!a.strong_only()) {
      for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        WeakUnifyFrame::Builder ub(A);
        ub->a1 = a;
        ub->a2 = b.head();
        alts.push(A,tail.add(A,Frame(ub.build())).build());
      }
    }
    
    // assume that <a> doesn't occur in the path or lemmas
    {
      // add constraints (wrt path)
      if(!a.strong_only()) {
        for(auto b = f->next.false_; !b.empty(); b = b.tail())
          if(!state->val.push_constraint(A,OrderAtom::neq(A,a,b.head()))) return alts;
      }
      WeakConnectionsFrame::Builder cb(A);
      cb->nodes_limit = f->nodes_limit;
      cb->atoms = f->atoms.tail();
      cb->branches = f->branches.add(A,Branch{
        .false_ = f->next.false_.add(A,a),
        .true_ = f->next.true_,
      });
      cb->branch_count = f->branch_count + 1;
      cb->next = Branch {
        .false_ = f->next.false_,
        .true_ = f->next.true_.add(A,a),
      };
      alts.push(A,builder().add(A,Frame(cb.build())).build());
    }
  } else if(f->branch_count) {
    WeakSetFrame::Builder b(A);
    b->nodes_limit = f->nodes_limit;
    b->branches = f->branches;
    b->branch_count = f->branch_count;
    alts.push(A,builder().add(A,Frame(b.build())).build());
  } else {
    alts.push(A,builder().build());
  }
  return alts;
}
