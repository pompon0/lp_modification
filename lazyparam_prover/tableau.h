#ifndef TABLEAU_H_
#define TABLEAU_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/pred_format.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/stack.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"


struct Ineq { Term l,r; };

struct TabState {
  Snapshot snapshot;
  Valuation mgu_state;
  vec<Ineq> ineqs;
  vec<OrClause> clauses_used;
  int nodes_used;
  List<Bud> buds;
};

/*
struct TabState {
  
  inline bool validate_lt(Node l, Node r) {
    return !kbo(mgu_state.eval(r),mgu_state.eval(l));
  }

  inline bool validate_acyclic() {
    //TODO
    return 1;
  }

  inline bool add_eq(Node l, Node r) {
    if(!mgu_state.mgu(l,r)) return 0;
    for(auto &i : ineqs) if(!validate_lt(i.l,i.r)) return 0;
    return validate_acyclic();
  }

  inline bool add_lt(Node l, Node r) {
    ineqs.push_back({l,r});
    return validate_lt(l,r) && validate_acyclic();
  }
};
*/
struct SearchState {
  SearchState(OrForm _form, int _nodes_limit) : 
    form(_form),
    nodes_limit(_nodes_limit) {}
  
  //reductionOrder;
  const NotAndForm form;
  int nodes_limit;
  vec<TabState> tabs;

  void start() { FRAME("start()");
    for(auto cla : form.or_clauses) {
      // start will all-negative clauses
      bool ok = 1;
      for(auto a : cla.atoms) ok &= !a.sign();
      if(!ok) continue;

      // allocate vars and push initial buds
      tabs.emplace_back();
      auto &tab = tabs.back();
      cla = tab.mgu_state.alloc_vars(cla);
      tab.clauses_used.push_back(cla);
      for(auto a : cla.atoms) tab.buds += Bud{Branch(a)};
      tab.snapshot = stack;
    }
  }

  void expand(TabState tab) { FRAME("expand()");
    if(++tab.nodes_used>nodes_limit) return;
    for(auto cla : form.or_clauses) {
      // allocate vars
      TabState tab_cla = tab;
      cla = tab_cla.mgu_state.alloc_vars(cla);
      tab_cla.clauses_used.push_back(cla);
      // pop first branch
      auto branch = tab_cla.buds.head().branch;
      tab_cla.buds = tab_cla.buds.tail();
      // each iteration generates an alternative
      for(size_t i=0; i<cla.atoms.size(); ++i) {
        // unify strong atom (strong bud)
        TabState tab_strong = tab_cla;
        if(!tab_strong.mgu_state.opposite(branch.head(),cla.atoms[i])) continue;
        // Push new weak buds
        for(size_t j=0; j<cla.atoms.size(); ++j) if(i!=j) {
          tab_strong.buds += Bud{cla.atoms[j]+branch};
        }
        tab_strong.snapshot = stack;
        tabs.push_back(tab_strong);
      }
    }
  }

  void weak_pred(TabState tab_initial) { FRAME("weak_pred()");
    auto b1 = tab_initial.buds.head().branch;
    tab_initial.buds = tab_initial.buds.tail();
    for(auto b2 = b1.tail(); !b2.empty(); b2 = b2.tail()) {
      auto tab = tab_initial;
      if(!tab.mgu_state.opposite(b1.head(),b2.head())) continue;
      tab.snapshot = stack;
      tabs.push_back(tab);
    }
  }

  ptr<Proof> step(){ FRAME("step()");
    DEBUG info("step(): tabs.size() = %",tabs.size());
    // pop first tab
    auto tab = tabs.back(); tabs.pop_back();
    // if all branches are closed, then we found a proof
    if(tab.buds.empty()){
      ptr<Proof> proof(new Proof);
      for(auto cla : tab.clauses_used)
        proof->and_clauses.push_back(ground(tab.mgu_state.eval(cla)).neg());
      return proof;
    }
    // reset stack
    stack = tab.snapshot;
    expand(tab);
    // try weak closing after expanding, so that the closed variant will be processed first.
    // weak_pred will work fine with just 1 elem in the branch:
    // if will just have nothing to match against
    weak_pred(tab);
    return 0;
  }
};

ptr<Proof> prove(OrForm form, size_t limit) { FRAME("prove()");
  SearchState s(form,limit);
  s.start();
  for(size_t steps = 0; s.tabs.size(); steps++) {
    DEBUG if(steps%1000==0) info("steps = %",steps);
    if(auto proof = s.step()) return proof;
  }
  return 0;
}

ptr<Proof> prove_loop(OrForm form, size_t limit) { FRAME("prove_loop()");
  form = append_eq_axioms(form);
  for(size_t i=1; i<=limit; ++i) {
    DEBUG info("limit = %",i);
    if(auto proof = prove(form,i)) {
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*proof));
      return proof;
    }
  }
  DEBUG info("FAILURE");
  return 0;
}

#endif  // TABLEAU_H_
