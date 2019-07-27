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

using Branch = List<Atom>;

inline str show(Branch b) {
  vec<str> atoms;
  for(; !b.empty(); b = b.tail()) atoms.push_back(show(b.head()));
  return util::fmt("[%]",util::join(", ",atoms));
}

//////////////////////////////////////////

struct SearchState {
  SearchState(OrForm _form) : form(_form) {}
  
  const NotAndForm form;
  Valuation val;
  size_t nodes_used = 0;
  List<OrClause> clauses_used;

  ptr<Proof> get_proof() {
    ptr<Proof> proof(new Proof);
    for(auto l=clauses_used; !l.empty(); l = l.tail()) {
      proof->and_clauses.push_back(ground(val.eval(l.head())).neg());
    }
    return proof;
  }

  struct Snapshot {
    Valuation::Snapshot val;
    ::Snapshot stack;
    size_t nodes_used;
    List<OrClause> clauses_used;
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
  struct _WeakSetFrame;
  struct _WeakFrame;
  struct _WeakUnifyFrame;

  struct Frame {
  public:
    enum Type { START, STRONG, WEAK_SET, WEAK, WEAK_UNIFY };
    Type type() const { return Type(*LType::at(ptr)); }
  private:
    using LType = Lens<size_t,0>;
    enum { SIZE = LType::END };
    uint8_t *ptr;
    explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

    friend Variant<Frame,START,_StartFrame>;
    friend Variant<Frame,STRONG,_StrongFrame>;
    friend Variant<Frame,WEAK_SET,_WeakSetFrame>;
    friend Variant<Frame,WEAK,_WeakFrame>;
    friend Variant<Frame,WEAK_UNIFY,_WeakUnifyFrame>;
  };
  
  List<Frame> frames;
  bool done(){ return frames.empty(); }

  template<typename Alts> void run(State &state, Alts alts) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: start(state,StartFrame(f),alts); break;
      case Frame::STRONG: strong(state,StrongFrame(f),alts); break;
      case Frame::WEAK_SET: weak_set(state,WeakSetFrame(f),alts); break;
      case Frame::WEAK: weak(state,WeakFrame(f),alts); break;
      case Frame::WEAK_UNIFY: weak_unify(state,WeakUnifyFrame(f),alts); break;
      default: error("f.type() = %",f.type());
    }
  }

  struct _StartFrame { size_t nodes_limit; };
  using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

  template<typename Alts> void start(State &state, StartFrame f, Alts alts) const { FRAME("start");
    for(auto cla : state.form.or_clauses) {
      // start will all-negative clauses
      bool ok = 1;
      for(size_t i=cla.atom_count(); i--;) ok &= !cla.atom(i).sign();
      if(!ok) continue;
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = Branch();
      b->cla = cla;
      b->strong_id = -1;
      alts(Cont{List<Frame>(Frame(b.build()))});
    }
  }
  
  struct _StrongFrame {
    size_t nodes_limit;
    Branch branch;
    OrClause cla;
    ssize_t strong_id;
  };
  using StrongFrame = Variant<Frame,Frame::STRONG,_StrongFrame>;

  template<typename Alts> void strong(State &state, StrongFrame f, Alts alts) const { FRAME("strong(%,%)",show(f->cla),f->strong_id);
    SCOPE("strong");
    auto cla = f->cla.shift(state.val.size());
    // do not use f->cla from now on
    state.val.resize(cla.var_count());
    if(f->strong_id>=0 && !state.val.opposite(f->branch.head(),cla.atom(f->strong_id))) return;
    state.clauses_used = cla + state.clauses_used;
    size_t branch_count = cla.atom_count()-(f->strong_id>=0);
    if(!branch_count){ alts(Cont{frames.tail()}); return; }
    List<Branch> branches;
    for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) branches += cla.atom(i) + f->branch;
    WeakSetFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branch_count = branch_count;
    b->branches = branches;
    alts(Cont{Frame(b.build()) + frames.tail()});
  }

  struct _WeakSetFrame {
    size_t nodes_limit;
    size_t branch_count;
    List<Branch> branches;
  };
  using WeakSetFrame = Variant<Frame,Frame::WEAK_SET,_WeakSetFrame>;

  template<typename Alts> void weak_set(State &state, WeakSetFrame f, Alts alts) const { FRAME("weak_set");
    SCOPE("weak_set");
    DEBUG if(!f->branch_count) error("f->branch_count = 0");
    if(f->branch_count==1){
      WeakFrame::Builder b;
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
      wb->nodes_limit = f->nodes_limit;
      wb->branch = f->branches.head();
      alts(Cont{Frame(wsb.build()) + (Frame(wb.build()) + frames.tail())});
    }
    WeakFrame::Builder wb;
    wb->nodes_limit = state.nodes_used + per_bud;
    wb->branch = f->branches.head();
    WeakSetFrame::Builder wsb;
    wsb->nodes_limit = f->nodes_limit;
    wsb->branch_count = f->branch_count-1;
    wsb->branches = f->branches.tail();
    alts(Cont{Frame(wb.build()) + (Frame(wsb.build()) + frames.tail())});
  }

  struct _WeakFrame { size_t nodes_limit; Branch branch; };
  using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

  template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.head()));
    SCOPE("weak");
    if(state.nodes_used<f->nodes_limit) {
      COUNTER("expand");
      state.nodes_used++;
      for(auto cla : state.form.or_clauses)
        for(size_t i=cla.atom_count(); i--;) {
          StrongFrame::Builder b;
          b->nodes_limit = f->nodes_limit;
          b->branch = f->branch;
          b->cla = cla;
          b->strong_id = i;
          alts(Cont{Frame(b.build()) + frames.tail()});
        }
      // WARNING: we are manually rewinding the state here:
      state.nodes_used--;
    }
    for(auto b2 = f->branch.tail(); !b2.empty(); b2 = b2.tail()) {
      WeakUnifyFrame::Builder b;
      b->a1 = f->branch.head();
      b->a2 = b2.head();
      if(b->a1.pred()!=b->a2.pred()) continue; // 16s -> 15s
      alts(Cont{Frame(b.build())+frames.tail()});
    }
  }
  
  struct _WeakUnifyFrame { Atom a1,a2; };
  using WeakUnifyFrame = Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
  template<typename Alts> void weak_unify(State &state, WeakUnifyFrame f, Alts &alts) const { FRAME("weak_unify");
    SCOPE("weak_unify");
    if(state.val.opposite(f->a1,f->a2)) alts(Cont{frames.tail()}); 
  }  
};

ptr<Proof> prove(OrForm form, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(form);
  Cont::StartFrame::Builder b;
  b->nodes_limit = limit;
  if(alt::search(s,Cont{List<Cont::Frame>(Cont::Frame(b.build()))})) {
    return s.get_proof();
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