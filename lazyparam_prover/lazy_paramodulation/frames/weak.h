struct _WeakFrame {
  Branch branch;

  template<typename Div> INL void run(Div *d) const { STATE_FRAME(d->A,d->state,"weak(%)",show(branch.false_.head())); 
    d->state->stats.weak_steps++;
    COUNTER("expand");
    // match lemma
    if(matches_lemma(*d->state,branch)){ 
      STATE_FRAME(d->A,d->state,"matches_lemma");
      d->alt([&](typename Div::Alt *x){ x->feature_branch(branch); });
      return;
    }
    auto a = branch.false_.head();
    // reduce
    if(a.pred()==Atom::EQ && a.sign()) {
      d->alt([&](typename Div::Alt *x){
        auto br = branch;
        x->feature_branch(br);
        x->task([br,a](Div *d)INLL{
          if(!d->state->val.unify(d->A,a.arg(0),a.arg(1))) return;
          d->state->lazy_clauses_used.push(d->A,lazy(d->A,AxiomClause{AndClause::make(d->A,a.neg())}));
          d->alt([&](typename Div::Alt *x){ x->feature_branch(br); });
        });
      });
    }
    // weak connections
    for(auto b = branch.false_.tail(); !b.empty(); b = b.tail()) {
      try_weak_match(d,a,b.head());
      try_weak_param(d,a,b.head());
      try_weak_param(d,b.head(),a);
    }
    auto budget = d->size_limit() - d->state->nodes_used;
    // strong connections
    // P(r),-P(s)
    if(a.pred()!=Atom::EQ) {
      STATE_FRAME(d->A,d->state,"[%] %",a.id(),show(a));
      // since a may have been paramodulated, we cannot use the most optimized
      // version of get_matches.
      auto matches = d->state->cla_index.get_matches(a.pred(),!a.sign(),budget);
      while(auto mca = matches.next()) {
        auto ca = mca.get();
        DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
        d->alt([&](typename Div::Alt *x){
          auto br = branch;
          x->feature_branch(br);
          x->task([br,ca](Div *d)INLL{ strong(d,br,ca.cla,ca.i); });
        });
      }
    }
    // L[p],l/=r
    auto matches = d->state->cla_index.get_matches(Atom::EQ,false,budget);
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
      d->alt([&](typename Div::Alt *x){
        auto br = branch;
        x->feature_branch(br);
        x->task([ca,br](Div *d)INLL{
          _LazyPreStrongConnectionFrame{
            .branch = br,
            .dcla = ca.cla,
            .strong_id = ca.i,
            .branch_lr = false,
          }.run(d);
        });
      });
    }
    if(a.pred()==Atom::EQ && !a.sign()) {
      // l/=r,L[p]
      auto matches = d->state->cla_index.get_all(budget);
      while(auto mca = matches.next()) {
        auto ca = mca.get();
        DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
        d->alt([&](typename Div::Alt *x){
          auto br = branch;
          x->feature_branch(br);
          x->task([ca,br](Div *d)INLL{
            _LazyPreStrongConnectionFrame{
              .branch = br,
              .dcla = ca.cla,
              .strong_id = ca.i,
              .branch_lr = true,
            }.run(d);
          });
        });
      }
    }
  }

  // l/=r,...,a = L[p]
  template<typename Div> INL void try_weak_param(Div *d, Atom lr, Atom L) const {
    if(lr.pred()!=Atom::EQ || lr.sign()) return;
    auto br = branch;
    d->alt([&](typename Div::Alt *x)INLL{
      x->feature_branch(br);
      x->task([br,lr,L](Div *d)INLL{
        _LazyPreWeakConnectionFrame{
          .branch=br,
          .L = L,
          .lr = lr,
        }.run(d);
      });
    });
  }

  // weak connection: -P(r), ..., a = P(s)
  template<typename Div> INL void try_weak_match(Div *d, Atom x, Atom y) const {
    if(x.pred()==Atom::EQ) return;
    if(Index::atom_hash(x)!=(Index::atom_hash(y)^1)) return;
    auto br = branch;
    d->alt([&](typename Div::Alt *a)INLL{
      a->feature_branch(br);
      a->task([br,x,y](Div *d)INLL{
        d->state->stats.weak_unify_steps++;
        if(!d->state->val.unify(d->A,x,y)) return;
        d->alt([&](typename Div::Alt *a)INLL{
          a->feature_branch(br);
        });
      });
    });
  }
};

INL static bool matches_lemma(State &state, Branch branch) {
  auto a = branch.false_.head();
  auto atom_hash = Index::atom_hash(a);
  for(auto b = branch.true_; !b.empty(); b = b.tail()) {
    if(atom_hash!=Index::atom_hash(b.head())) continue;
    if(state.val.equal_mod_sign(a,b.head())) return true;
  }
  return false;
}
