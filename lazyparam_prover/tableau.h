#ifndef TABLEAU_H_
#define TABLEAU_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/variant.h"
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
#include "lazyparam_prover/prover_output.h"

namespace tableau {

struct Branch {
  List<Atom> false_;
  List<Atom> true_;
};

inline str show(Branch b) {
  vec<str> atoms;
  for(auto bt = b.false_; !bt.empty(); bt = bt.tail()) atoms.push_back(show(bt.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

//////////////////////////////////////////

struct SearchState {
  SearchState(const ClauseIndex &_cla_index) : cla_index(&_cla_index) {}
 
  ClauseIndex::State cla_index;

  KBO val;
  size_t nodes_used = 0;
  List<DerAndClause> clauses_used;

  Stats stats;

  AndClause allocate(DerAndClause dcla) { FRAME("strong_unify()");
    dcla = val.allocate(dcla);
    clauses_used += dcla;
    nodes_used += dcla.cost();
    for(size_t i=dcla.constraint_count(); i--;){
      val.push_constraint(dcla.constraint(i));
    }
    return dcla.derived();
  }

  // cannot return the proto, because parsing context is not available.
  // This means that Valuation has to be included in the ProverOutput.
  ptr<OrForm> get_proof() {
    auto proof = util::make<OrForm>();
    for(auto l=clauses_used; !l.empty(); l = l.tail()) {
      proof->and_clauses.push_back(l.head());
    }
    return proof;
  }

  struct Snapshot {
    KBO::Snapshot val;
    tableau::Snapshot stack;
    size_t nodes_used;
    List<DerAndClause> clauses_used;
    ClauseIndex::State cla_index;
  };

  void rewind(Snapshot s) {
    val.rewind(s.val);
    stack = s.stack;
    nodes_used = s.nodes_used;
    clauses_used = s.clauses_used;
    cla_index = s.cla_index;
  }

  Snapshot snapshot(){
    return {val.snapshot(),stack,nodes_used,clauses_used,cla_index};
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
    while(auto dcla = state.cla_index.next_starting_clause()) {
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = Branch();
      b->dcla = dcla.get();
      b->strong_id = -1;
      alts(Cont{List<Frame>(Frame(b.build()))});
    }
  }

  Maybe<List<Atom>> strong_resolution(State &state, size_t nodes_limit, List<Atom> todo) const { FRAME("strong_resolution()");
    List<Atom> checked;
    while(!todo.empty()) {
      auto a = todo.head();
      todo = todo.tail();
      // Look for strong only atoms.
      if(!a.strong_only()) { checked += a; continue; }
      size_t budget = nodes_limit - state.nodes_used;
      auto filter = state.cla_index.get_matches(a,budget);
      auto mca = filter.next();
      if(!mca) return nothing();
      // Filter out those which have more that 1 possible unification.
      if(filter.next()) { checked += a; continue; }
      state.stats.strong_only_steps++;
      auto ca = mca.get();
      // Connect the new clause and analyze the new atoms recursively.
      auto cla = state.allocate(ca.cla);
      if(!state.val.mgu(a,cla.atom(ca.i))) return nothing();
      for(size_t i=cla.atom_count(); i--;) if(i!=ca.i) todo += cla.atom(i);
    }
    return just(checked);
  }

  struct _StrongFrame {
    size_t nodes_limit;
    Branch branch;
    DerAndClause dcla;
    ssize_t strong_id;
  };
  using StrongFrame = Variant<Frame,Frame::STRONG,_StrongFrame>;
  template<typename Alts> void strong(State &state, StrongFrame f, Alts alts) const { FRAME("strong(%,%)",show(f->dcla),f->strong_id);
    state.stats.strong_steps++;
    auto cla = state.allocate(f->dcla);
    if(f->strong_id>=0) if(!state.val.mgu(f->branch.false_.head(),cla.atom(f->strong_id))) return;

    List<Atom> todo;
    for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) todo += cla.atom(i);
    auto matoms = strong_resolution(state,f->nodes_limit,todo);
    if(!matoms) return;
    
    WeakConnectionsFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->atoms = matoms.get();
    b->branches = List<Branch>();
    b->branch_count = 0;
    b->next = f->branch;
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
      // TODO: add constraints preventing clauses becoming contradictory
      auto atom_hash = Index::atom_hash(a);
      WeakConnectionsFrame::Builder cb;
      cb->nodes_limit = f->nodes_limit;
      cb->atoms = f->atoms.tail();
      cb->branches = f->branches;
      cb->branch_count = f->branch_count;
      cb->next = f->next;
      auto tail = Frame(cb.build())+frames.tail();
      // try to match with lemma
      // TODO: lazy lemma - keep track of constraints which will allow you to close a branch early
      for(auto b = f->next.true_; !b.empty(); b = b.tail()) {
        if(atom_hash!=Index::atom_hash(b.head())) continue;
        if(!state.val.equal_mod_sign(a,b.head())) continue;
        WeakConnectionsFrame::Builder cb;
        *cb = *f;
        cb->atoms = f->atoms.tail();
        alts(Cont{Frame(cb.build())+frames.tail()});
        return;
      }
      // try to unify with path
      if(!a.strong_only()) {
        for(auto b = f->next.false_; !b.empty(); b = b.tail()) {
          if((atom_hash^1)!=Index::atom_hash(b.head())) continue;
          WeakUnifyFrame::Builder ub;
          ub->a1 = a;
          ub->a2 = b.head();
          alts(Cont{Frame(ub.build())+tail});
        }
      }
      
      // assume that <a> doesn't occur in the path or lemmas
      {
        // add constraints (wrt path)
        if(!a.strong_only()) {
          for(auto b = f->next.false_; !b.empty(); b = b.tail())
            if(!state.val.push_constraint(OrderAtom::neq(a,b.head()))) return;
        }
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

  template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.false_.head())); 
    size_t budget = f->nodes_limit - state.nodes_used;
    COUNTER("expand");
    if(budget<f->min_cost) return;
    List<Frame> tail = frames.tail();
    if(f->min_cost) {
      MinCostFrame::Builder b;
      b->min_cost = state.nodes_used + f->min_cost;
      tail += Frame(b.build());
    }
    
    auto matches = state.cla_index.get_matches(f->branch.false_.head(),budget);
    while(auto mca = matches.next()) {
      auto ca = mca.get();
      DEBUG if(ca.cla.cost()>budget) error("ca.cla.cost()>budget");
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
  return {
    res.cont_count,
    limit,
    s.val,
    res.found ? s.get_proof() : 0,
    s.stats,
  };
}

ProverOutput prove_loop(const Ctx &ctx, OrForm form) { FRAME("prove_loop()");
  SCOPE("prove_loop"); 
  Stats stats;
  size_t cont_count = 0;
  size_t limit = 0;
  //info("ClauseIndex begin");
  ClauseIndex idx(form);
  //info("ClauseIndex end");
  for(;!ctx.done();) {
    limit++; // avoid incrementing limit before context check
    DEBUG info("limit = %",limit);
    ProverOutput out = prove(ctx,idx,limit);
    out.cont_count += cont_count;
    out.stats += stats;
    if(out.proof) {
      DEBUG info("SUCCESS");
      DEBUG info("%",show(*out.proof));
      return out;
    }
    stats = out.stats;
    cont_count = out.cont_count;
    //std::cerr << "expands[" << limit << "]: " << profile.scopes["expand"].count << std::endl;
  }
  DEBUG info("FAILURE");
  ProverOutput out;
  out.cont_count = cont_count;
  out.cost = limit;
  out.stats = stats;
  return out; 
}

} // namespace tableau

#endif  // TABLEAU_H_
