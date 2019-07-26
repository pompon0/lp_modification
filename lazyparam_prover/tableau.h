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
private:
  Valuation valuation_;
  List<OrClause> clauses_used_;
  size_t nodes_used_;
  List<BudSet> bud_sets_;
  Snapshot snapshot;
public:
  TabState(OrClause cla) : valuation_(cla.var_count()) {
    clauses_used_ = List<OrClause>(cla);
    nodes_used_ = 1;
    snapshot = stack;
  }

  void rewind() const { stack = snapshot; }

  Valuation valuation() const { return valuation_; }
  List<OrClause> clauses_used() const { return clauses_used_; }
  size_t nodes_used() const { return nodes_used_; }
  List<BudSet> bud_sets() const { return bud_sets_; }

  TabState push_clause(OrClause cla) const {
    TabState t = *this;
    t.clauses_used_ = cla + clauses_used_;
    t.nodes_used_ = nodes_used_+1;
    t.snapshot = stack;
    return t;
  }

  TabState push_bud_set(BudSet bs) const {
    TabState t = *this;
    t.bud_sets_ += bs;
    t.snapshot = stack;
    return t;
  }

  TabState pop_bud_set() const {
    TabState t = *this;
    t.bud_sets_ = t.bud_sets_.tail();
    return t;
  }

  TabState set_valuation(Valuation _valuation) const {
    TabState t = *this;
    t.valuation_ = _valuation;
    t.snapshot = stack;
    return t;
  }

  Bud top_bud() const {
    auto bs = bud_sets_.head();
    DEBUG if(bs.branches_count!=1) error("branches_count = % want %",bs.branches_count,1);
    return Bud{bs.nodes_limit,bs.branches.head()};
  }

  TabState pop_bud() const { FRAME("pop_bud()");
    top_bud(); // ensure that top bud_set has only one bud
    TabState t = *this; 
    t.bud_sets_ = t.bud_sets_.tail();
    return t;
  }
};

inline str show(TabState tab) {
  vec<str> bss;
  for(auto bs = tab.bud_sets(); !bs.empty(); bs = bs.tail()) {
    bss.push_back(util::to_str(bs.head().branches_count));
  }
  if(bss.size()) bss[0] = show(tab.bud_sets().head());
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
      for(size_t i=cla.atom_count(); i--;) ok &= !cla.atom(i).sign();
      if(!ok) continue;

      // allocate vars and push initial buds
      all(TabState(cla),Bud{nodes_limit,Branch()},cla,-1);
    }
  }

  TabState pop_tab() { FRAME("pop_tab()");
    DEBUG if(!tabs.size()) error("!tab.size()");
    TabState tab = tabs.back(); tabs.pop_back();
    tab.rewind();
    if(tab.bud_sets().empty()) return tab;
    DEBUG if(tab.bud_sets().head().branches_count<1) error("branches_count<1");
    if(tab.bud_sets().head().branches_count==1) return tab;
    // if there is more than one branch in the top bud set, we need to break it
    auto bs = tab.bud_sets().head(); 
    tab = tab.pop_bud_set();
    
    size_t per_bud = (bs.nodes_limit-tab.nodes_used())/bs.branches_count;
    if(bs.nodes_limit>tab.nodes_used()) { // if there is budget to allocate.
      tabs.push_back(tab
        .push_bud_set(BudSet(bs.nodes_limit,1,List<Branch>(bs.branches.head())))
        .push_bud_set(BudSet(bs.nodes_limit-(per_bud+1),bs.branches_count-1,bs.branches.tail())));
    } 
    // otherwise there is only one option
    return tab
        .push_bud_set(BudSet(bs.nodes_limit,bs.branches_count-1,bs.branches.tail()))
        .push_bud_set(BudSet(tab.nodes_used()+per_bud,1,List<Branch>(bs.branches.head())));
  }

  void all(TabState tab, Bud bud, OrClause cla, ssize_t avoid) {
    size_t branch_count = cla.atom_count()-(avoid>=0);
    if(branch_count) {
      List<Branch> branches;
      for(ssize_t i=cla.atom_count(); i--;) if(i!=avoid) branches += cla.atom(i) + bud.branch;
      tab = tab.push_bud_set(BudSet(bud.nodes_limit,branch_count,branches));
    }
    tabs.push_back(tab);
  }

  /*void all(const TabState &tab0, Bud bud, const vec<Atom> &atoms) {
    tabs.emplace_back(tab0);
    auto &tab = tabs.back();
    for(size_t j=0; j<atoms.size(); ++j) tab.buds += Bud{bud.nodes_limit,atoms[j]+bud.branch};
    tab.snapshot = stack;
  }*/

  //TODO: implement reversible & extendable array
  //TODO: branch immediately to use a single global valuation
  //TODO: index clauses by strong atom predicate
  //TODO: implement custom brand modification
  void expand(TabState tab) { FRAME("expand()");
    // pop first branch
    auto bud = tab.top_bud(); tab = tab.pop_bud();
    if(tab.nodes_used()>=bud.nodes_limit) return;
    SCOPE("expand");
    auto branch = bud.branch;
    for(auto cla : form.or_clauses) {
      SCOPE("expand : clause");
      cla = cla.shift(tab.valuation().size());
      { SCOPE("expand : clause : strong atom");
      // each iteration generates an alternative
      for(size_t i=cla.atom_count(); i--;) {
        // unify strong atom (strong bud)
        Valuation::Builder b(tab.valuation(),cla.var_count());
        COUNTER("expand : clause : strong atom : opposite");
        if(!b.opposite(branch.head(),cla.atom(i))) continue;
        COUNTER("expand : clause : strong atom : opposite done");
        // Push new weak buds
        all(tab.push_clause(cla).set_valuation(b.build()),bud,cla,i);
      }
      }
    }
  }

  void weak_pred(TabState tab) { FRAME("weak_pred()");
    SCOPE("weak_pred");
    auto b1 = tab.top_bud().branch;
    tab = tab.pop_bud();
    for(auto b2 = b1.tail(); !b2.empty(); b2 = b2.tail()) {
      COUNTER("weak_pred::loop: opposite()");
      Valuation::Builder b(tab.valuation());
      if(!b.opposite(b1.head(),b2.head())) continue;
      tabs.push_back(tab.set_valuation(b.build()));
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
    if(tab.bud_sets().empty()){
      ptr<Proof> proof(new Proof);
      for(auto cl = tab.clauses_used(); !cl.empty(); cl = cl.tail())
        proof->and_clauses.push_back(ground(tab.valuation().eval(cl.head())).neg());
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
