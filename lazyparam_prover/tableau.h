#ifndef TABLEAU_H_
#define TABLEAU_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/pred_format.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/parse.h"
#include "lazyparam_prover/eq_axioms.h"

using Branch = List<Atom>;

struct Bud {
  size_t nodes_limit;
  Branch branch;
};

struct BudSet {
  BudSet(size_t _nodes_limit, size_t _branches_count, List<Branch> _branches) : nodes_limit(_nodes_limit), branches_count(_branches_count), branches(_branches) {
    //DEBUG if(branches_count!=branches.size()) error("branches_count = %, brances.size() = %",branches_count,branches.size());
  }
  size_t nodes_limit;
  size_t branches_count;
  List<Branch> branches;
};

inline str show(Branch b) {
  vec<str> atoms;
  for(; !b.empty(); b = b.tail()) atoms.push_back(show(b.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

inline str show(List<Bud> buds) {
  vec<str> branches;
  for(; !buds.empty(); buds = buds.tail()) branches.push_back(show(buds.head().branch)+"\n");
  return util::join("",branches);
}

inline str show(BudSet bs) {
  vec<str> branches; for(auto b = bs.branches; !b.empty(); b = b.tail()) branches.push_back(show(b.head()));
  return util::fmt("{nodes_limit = %, branches_count = %, branches = [%]",bs.nodes_limit,bs.branches_count,util::join(",",branches));
}

//////////////////////////////////////////

struct Ineq { Term l,r; };

struct TabState {
  Snapshot snapshot;
  Valuation mgu_state;
  List<OrClause> clauses_used;
  size_t nodes_used = 0;
  List<BudSet> bud_sets;

  TabState(const OrClause &cla) :
    snapshot(stack),
    mgu_state(cla.var_count),
    clauses_used(List<OrClause>(cla)),
    nodes_used(1),
    bud_sets(List<BudSet>()) {}

  TabState(const TabState &tab, List<OrClause> _clauses_used) :
    snapshot(stack),
    mgu_state(tab.mgu_state,_clauses_used.head().var_count),
    clauses_used(_clauses_used),
    nodes_used(tab.nodes_used),
    bud_sets(tab.bud_sets) {}

  Bud pop_bud() { FRAME("pop_bud()");
    auto bs = bud_sets.head();
    DEBUG if(bs.branches_count!=1) error("branches_count = % want %",bs.branches_count,1);
    bud_sets = bud_sets.tail();
    return Bud{bs.nodes_limit,bs.branches.head()};
  }
};

inline str show(const TabState &tab) {
  vec<str> bss;
  for(auto bs = tab.bud_sets; !bs.empty(); bs = bs.tail()) {
    bss.push_back(util::to_str(bs.head().branches_count));
  }
  if(bss.size()) bss[0] = show(tab.bud_sets.head());
  return util::fmt("{ bud_sets = [%] }", util::join(",",bss));
}

struct SearchState {
  SearchState(OrForm _form) : form(_form) {}
  
  const NotAndForm form;
  vec<TabState> tabs;

  void start(size_t nodes_limit) { FRAME("start()");
    for(auto cla : form.or_clauses) {
      // start will all-negative clauses
      bool ok = 1;
      for(auto a : cla.atoms) ok &= !a.sign();
      if(!ok) continue;

      // allocate vars and push initial buds
      TabState tab(cla);
      all(tab,Bud{nodes_limit,Branch()},cla.atoms);
    }
  }

  TabState pop_tab() { FRAME("pop_tab()");
    DEBUG if(!tabs.size()) error("!tab.size()");
    TabState ht = tabs.back();
    stack = ht.snapshot;
    if(ht.bud_sets.empty()) return ht;
    DEBUG if(ht.bud_sets.head().branches_count<1) error("branches_count<1");
    if(ht.bud_sets.head().branches_count==1){ tabs.pop_back(); return ht; }
    // if there is more than one branch in the top bud set, we need to break it
    TabState &tt = tabs.back();
    
    auto bs = ht.bud_sets.head();
    size_t per_bud = (bs.nodes_limit-ht.nodes_used)/bs.branches_count;
    if(bs.nodes_limit>ht.nodes_used) { // if there is budget to allocate.
      tt.bud_sets = 
          BudSet(bs.nodes_limit-(per_bud+1),bs.branches_count-1,bs.branches.tail())
        + (BudSet(bs.nodes_limit,1,List<Branch>(bs.branches.head())) + tt.bud_sets.tail());
      tt.snapshot = stack;
    } else { // otherwise there is only one option
      tabs.pop_back();
    }
    ht.bud_sets =
        BudSet(ht.nodes_used+per_bud,1,List<Branch>(bs.branches.head()))
      + (BudSet(bs.nodes_limit,bs.branches_count-1,bs.branches.tail()) + ht.bud_sets.tail());
    ht.snapshot = stack;
    return ht;
  }

  void all(const TabState &tab0, Bud bud, const vec<Atom> &atoms) {
    tabs.emplace_back(tab0);
    if(atoms.size()) {
      List<Branch> branches;
      for(auto &a : atoms) branches += a + bud.branch;
      tabs.back().bud_sets += BudSet(bud.nodes_limit,atoms.size(),branches);
    }
    tabs.back().snapshot = stack;
  }

  /*void all(const TabState &tab0, Bud bud, const vec<Atom> &atoms) {
    tabs.emplace_back(tab0);
    auto &tab = tabs.back();
    for(size_t j=0; j<atoms.size(); ++j) tab.buds += Bud{bud.nodes_limit,atoms[j]+bud.branch};
    tab.snapshot = stack;
  }*/

  void expand(TabState tab) { FRAME("expand()");
    // pop first branch
    auto bud = tab.pop_bud();
    if(++tab.nodes_used>bud.nodes_limit) return;
    SCOPE("expand");
    auto branch = bud.branch;
    for(auto cla : form.or_clauses) {
      SCOPE("expand : clause");
      cla.shift(tab.mgu_state.val.size());
      auto clauses_used = cla + tab.clauses_used;
      { SCOPE("expand : clause : strong atom");
      // each iteration generates an alternative
      for(size_t i=0; i<cla.atoms.size(); ++i) {
        // unify strong atom (strong bud)
        TabState tab_strong(tab,clauses_used);
        if(!tab_strong.mgu_state.opposite(branch.head(),cla.atoms[i])) continue;
        COUNTER("expand : clause : strong atom : opposite done");
        // Push new weak buds
        vec<Atom> atoms; for(size_t j=0; j<cla.atoms.size(); ++j) if(j!=i) atoms.push_back(cla.atoms[j]);
        all(tab_strong,bud,atoms);
      }
      }
    }
  }

  void weak_pred(TabState tab_initial) { FRAME("weak_pred()");
    SCOPE("weak_pred");
    auto b1 = tab_initial.pop_bud().branch;
    for(auto b2 = b1.tail(); !b2.empty(); b2 = b2.tail()) {
      COUNTER("weak_pred::loop: opposite()");
      auto tab = tab_initial;
      if(!tab.mgu_state.opposite(b1.head(),b2.head())) continue;
      tab.snapshot = stack;
      tabs.push_back(tab);
    }
  }

  ptr<Proof> step(){ FRAME("step()");
    SCOPE("step");
    //DEBUG info("step(): tabs.size() = %",tabs.size());
    // pop first tab
    // DEBUG info("tabs.back() = %",show(tabs.back()));
    auto tab = pop_tab();
    //DEBUG info("tab = %",show(tab));
    // if all branches are closed, then we found a proof
    if(tab.bud_sets.empty()){
      ptr<Proof> proof(new Proof);
      for(auto cl = tab.clauses_used; !cl.empty(); cl = cl.tail())
        proof->and_clauses.push_back(ground(tab.mgu_state.eval(cl.head())).neg());
      return proof;
    }
    {
      SCOPE("step : expand");
      expand(tab);
    }
      // try weak closing after expanding, so that the closed variant will be processed first.
      // weak_pred will work fine with just 1 elem in the branch:
      // it will just have nothing to match against
    {
      SCOPE("step : weak_pred");
      weak_pred(tab);
    }
    return 0;
  }
};

ptr<Proof> prove(OrForm form, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(form);
  s.start(limit);
  for(size_t steps = 0; s.tabs.size(); steps++) {
    DEBUG if(steps%1000==0) info("steps = %",steps);
    if(auto proof = s.step()) return proof;
  }
  return 0;
}

ptr<Proof> prove_loop(OrForm form, size_t limit) { FRAME("prove_loop()");
  SCOPE("prove_loop");
  form = append_eq_axioms(form);
  for(size_t i=1; i<=limit; ++i) {
    DEBUG info("limit = %",i);
    if(auto proof = prove(form,i)) {
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*proof));
      return proof;
    }
    std::cerr << "expands[" << i << "]: " << profile.scopes["expand"].count << std::endl;
  }
  DEBUG info("FAILURE");
  return 0;
}

#endif  // TABLEAU_H_
