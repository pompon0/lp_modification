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
#include "lazyparam_prover/alt.h"
#include "lazyparam_prover/lazy.h"
#include "lazyparam_prover/ctx.h"
#include "lazyparam_prover/index.h"

namespace tableau {

struct Branch {
  List<Atom> true_;
  List<Atom> false_;
};

inline str show(Branch b) {
  vec<str> atoms;
  for(auto bt = b.true_; !bt.empty(); bt = bt.tail()) atoms.push_back(show(bt.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

//////////////////////////////////////////

struct SearchState {
  SearchState(const ClauseIndex &_cla_index) : cla_index(_cla_index) {}
 
  const ClauseIndex &cla_index;

  KBO val;
  size_t nodes_used = 0;
  List<DerOrClause> clauses_used;

  ptr<DerAndClause> get_proof() {
    ptr<DerAndClause> proof(new DerAndClause);
    for(auto l=clauses_used; !l.empty(); l = l.tail()) {
      proof->cost += l.head().cost();
      for(auto cla : l.head().source()) {
        proof->source.push_back(ground(val.eval(cla)).neg());
      }
    }
    return proof;
  }

  struct Snapshot {
    KBO::Snapshot val;
    tableau::Snapshot stack;
    size_t nodes_used;
    List<DerOrClause> clauses_used;
  };

  void rewind(Snapshot s) {
    val.rewind(s.val);
    stack = s.stack;
    nodes_used = s.nodes_used;
    clauses_used = s.clauses_used;
  }

  Snapshot snapshot(){
    return {val.snapshot(),stack,nodes_used,clauses_used};
  }
};

struct Cont { 
  using State = SearchState;
 
  struct _StartFrame;
  struct _StrongFrame;
  struct _WeakConnectionsFrame;
  struct _WeakSetFrame;
  struct _WeakFrame;
  struct _WeakUnifyFrame;
  struct _MinCostFrame;

  struct Frame {
  public:
    enum Type { START, STRONG, WEAK_CONNECTIONS, WEAK_SET, WEAK, WEAK_UNIFY, MIN_COST };
    Type type() const { return Type(*LType::at(ptr)); }
  private:
    using LType = Lens<size_t,0>;
    enum { SIZE = LType::END };
    uint8_t *ptr;
    explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

    friend Variant<Frame,START,_StartFrame>;
    friend Variant<Frame,STRONG,_StrongFrame>;
    friend Variant<Frame,WEAK_CONNECTIONS,_WeakConnectionsFrame>;
    friend Variant<Frame,WEAK_SET,_WeakSetFrame>;
    friend Variant<Frame,WEAK,_WeakFrame>;
    friend Variant<Frame,WEAK_UNIFY,_WeakUnifyFrame>;
    friend Variant<Frame,MIN_COST,_MinCostFrame>;
  };
  
  List<Frame> frames;
  bool done(){ return frames.empty(); }

  template<typename Alts> void run(State &state, Alts alts) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: start(state,StartFrame(f),alts); break;
      case Frame::STRONG: strong(state,StrongFrame(f),alts); break;
      case Frame::WEAK_CONNECTIONS: weak_connections(state,WeakConnectionsFrame(f),alts); break;
      case Frame::WEAK_SET: weak_set(state,WeakSetFrame(f),alts); break;
      case Frame::WEAK: weak(state,WeakFrame(f),alts); break;
      case Frame::WEAK_UNIFY: weak_unify(state,WeakUnifyFrame(f),alts); break;
      case Frame::MIN_COST: min_cost(state,MinCostFrame(f),alts); break;
      default: error("f.type() = %",f.type());
    }
  }

  struct _StartFrame { size_t nodes_limit; };
  using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

  template<typename Alts> void start(State &state, StartFrame f, Alts alts) const { FRAME("start");
    for(auto dcla : state.cla_index.form.or_clauses) {
      OrClause cla = dcla.derived();
      // start with all-negative clauses
      bool ok = 1;
      for(size_t i=cla.atom_count(); i--;) ok &= !cla.atom(i).sign();
      if(!ok) continue;
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = Branch();
      b->dcla = dcla;
      b->strong_id = -1;
      alts(Cont{List<Frame>(Frame(b.build()))});
    }
  }
  
  struct _StrongFrame {
    size_t nodes_limit;
    Branch branch;
    DerOrClause dcla;
    ssize_t strong_id;
  };
  using StrongFrame = Variant<Frame,Frame::STRONG,_StrongFrame>;

  template<typename Alts> void strong(State &state, StrongFrame f, Alts alts) const { FRAME("strong(%,%)",show(f->dcla),f->strong_id);
    auto dcla = f->dcla.shift(state.val.size());
    // do not use f->dcla from now on
    state.nodes_used += dcla.cost();
    auto cla = dcla.derived();
    state.val.resize(cla.var_count());
    for(auto c = dcla.constraints(); !c.empty(); c = c.tail()) {
      state.val.push_constraint(c.head());
    }
    if(f->strong_id>=0) if(!state.val.mgu(f->branch.true_.head(),cla.atom(f->strong_id))) return;
    state.clauses_used += dcla;
    
    WeakConnectionsFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->atoms = List<Atom>();
    b->branches = List<Branch>();
    b->branch_count = 0;
    b->next = f->branch;
    for(ssize_t i=0,n=cla.atom_count(); i<n; ++i) if(i!=f->strong_id) b->atoms += cla.atom(i);
    alts(Cont{Frame(b.build()) + frames.tail()});
  }

  struct _WeakConnectionsFrame {
    size_t nodes_limit;
    List<Atom> atoms;
    List<Branch> branches;
    size_t branch_count;
    Branch next;
  };
  using WeakConnectionsFrame = Variant<Frame,Frame::WEAK_CONNECTIONS,_WeakConnectionsFrame>;
  template<typename Alts> void weak_connections(State &state, WeakConnectionsFrame f, Alts alts) const { FRAME("weak_connections");
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
      for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
        if(atom_hash!=Index::atom_hash(b.head())) continue;
        if(!state.val.equal_mod_sign(a,b.head())) continue;
        WeakConnectionsFrame::Builder cb;
        *cb = *f;
        cb->atoms = f->atoms.tail();
        alts(Cont{Frame(cb.build())+frames.tail()});
        return;
      }
      // try to unify with path
      for(auto b = f->next.true_; !b.empty(); b = b.tail()) {
        if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
        WeakUnifyFrame::Builder ub;
        ub->a1 = a;
        ub->a2 = b.head();
        alts(Cont{Frame(ub.build())+tail});
      }
      
      // assume that <a> doesn't occur in the path or lemmas
      {
        // add constraints (wrt path)
        for(auto b = f->next.true_; !b.empty(); b = b.tail())
          if(!state.val.push_constraint(Constraint::neq(a,b.head()))) return;
        WeakConnectionsFrame::Builder cb;
        cb->nodes_limit = f->nodes_limit;
        cb->atoms = f->atoms.tail();
        cb->branches = Branch{a + f->next.true_, f->next.false_} + f->branches;
        cb->branch_count = f->branch_count + 1;
        cb->next = Branch { f->next.true_, a + f->next.false_};
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


  struct _WeakSetFrame {
    size_t nodes_limit;
    size_t branch_count;
    List<Branch> branches;
  };
  using WeakSetFrame = Variant<Frame,Frame::WEAK_SET,_WeakSetFrame>;

  template<typename Alts> void weak_set(State &state, WeakSetFrame f, Alts alts) const { FRAME("weak_set");
    DEBUG if(!f->branch_count) error("f->branch_count = 0");
    if(f->branch_count==1){
      WeakFrame::Builder b;
      b->min_cost = 0;
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branches.head();
      weak(state,b.build(),alts);
      return;
    }
    
    size_t per_bud = (f->nodes_limit-state.nodes_used)/f->branch_count;
    if(f->nodes_limit>state.nodes_used) { // if there is budget to allocate.
      WeakSetFrame::Builder wsb;
      wsb->nodes_limit = f->nodes_limit-(per_bud+1);
      wsb->branch_count = f->branch_count-1;
      wsb->branches = f->branches.tail();
      WeakFrame::Builder wb;
      wb->min_cost = per_bud+1;
      wb->nodes_limit = f->nodes_limit;
      wb->branch = f->branches.head();
      alts(Cont{Frame(wsb.build()) + (Frame(wb.build()) + frames.tail())});
    }
    WeakFrame::Builder wb;
    wb->min_cost = 0;
    wb->nodes_limit = state.nodes_used + per_bud;
    wb->branch = f->branches.head();
    WeakSetFrame::Builder wsb;
    wsb->nodes_limit = f->nodes_limit;
    wsb->branch_count = f->branch_count-1;
    wsb->branches = f->branches.tail();
    alts(Cont{Frame(wb.build()) + (Frame(wsb.build()) + frames.tail())});
  }

  struct _WeakFrame { size_t min_cost; size_t nodes_limit; Branch branch; };
  using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

  template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.true_.head())); 
    size_t budget = f->nodes_limit - state.nodes_used;
    COUNTER("expand");
    if(budget<f->min_cost) return;
    List<Frame> tail = frames.tail();
    if(f->min_cost) {
      MinCostFrame::Builder b;
      b->min_cost = state.nodes_used + f->min_cost;
      tail += Frame(b.build());
    }

    for(auto ca : state.cla_index(f->branch.true_.head())) {
      if(ca.cla.cost()>budget) break;
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branch;
      b->dcla = ca.cla;
      b->strong_id = ca.i;
      alts(Cont{Frame(b.build()) + tail});
    }
  }
  
  struct _WeakUnifyFrame { Atom a1,a2; };
  using WeakUnifyFrame = Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
  template<typename Alts> void weak_unify(State &state, WeakUnifyFrame f, Alts &alts) const { FRAME("weak_unify");
    if(state.val.mgu(f->a1,f->a2)) alts(Cont{frames.tail()}); 
  }

  struct _MinCostFrame { size_t min_cost; };
  using MinCostFrame = Variant<Frame,Frame::MIN_COST,_MinCostFrame>;
  template<typename Alts> void min_cost(State &state, MinCostFrame f, Alts &alts) const { FRAME("min_cost");
    if(state.nodes_used>=f->min_cost) alts(Cont{frames.tail()});
  }
};

ProverOutput prove(const Ctx &ctx, const ClauseIndex &cla_index, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(cla_index);
  Cont::StartFrame::Builder b;
  b->nodes_limit = limit;
  auto res = alt::search(ctx,s,Cont{List<Cont::Frame>(Cont::Frame(b.build()))});
  return {res.cont_count,limit,res.found ? s.get_proof() : 0};
}

ProverOutput prove_loop(const Ctx &ctx, OrForm form) { FRAME("prove_loop()");
  SCOPE("prove_loop");
  // TODO: restrict it further to the case when equality atoms are reachable from all possible starting clause sets
  // TODO: look for starting sets with unreachable clauses - iteratively eliminate them
  if(has_equality(form)) { 
    //form = reduce_monotonicity_and_append_eq_axioms(form);
    //form = append_eq_axioms_with_restricted_transitivity(form);
    //form = append_eq_axioms(form);
    form = lazy::conv(form);
  }
  size_t cont_count = 0;
  size_t limit = 1;
  //info("ClauseIndex begin");
  ClauseIndex idx((NotAndForm)form);
  //info("ClauseIndex end");
  for(;!ctx.done(); ++limit) {
    DEBUG info("limit = %",limit);
    ProverOutput out = prove(ctx,idx,limit);
    out.cont_count += cont_count;
    if(out.proof) {
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*out.proof));
      return out;
    }
    cont_count = out.cont_count;
    //std::cerr << "expands[" << limit << "]: " << profile.scopes["expand"].count << std::endl;
  }
  DEBUG info("FAILURE");
  return {cont_count,limit}; 
}

} // namespace tableau

#endif  // TABLEAU_H_
