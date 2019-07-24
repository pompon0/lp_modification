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
  struct _WeakSetFrame;
  struct _WeakFrame;
  struct _WeakUnifyFrame;

  struct Frame {
  public:
    enum Type { START, WEAK };
    Type type() const { return Type(*LType::at(ptr)); }
  private:
    using LType = Lens<size_t,0>;
    enum { SIZE = LType::END };
    uint8_t *ptr;
    explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

    friend Variant<Frame,START,_StartFrame>;
    friend Variant<Frame,WEAK,_WeakFrame>;
  };
  
  List<Frame> frames;
  bool done(){ return frames.empty(); }

  template<typename Alts> void run(State &state, Alts alts) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: start(state,StartFrame(f),alts); break;
      case Frame::WEAK: weak(state,WeakFrame(f),alts); break;
      default: error("f.type() = %",f.type());
    }
  }

  struct _StartFrame { size_t nodes_limit; };
  using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

  template<typename Alts> void start(State &state, StartFrame f, Alts alts) const { FRAME("start");
    auto s = state.snapshot();
    for(auto cla : state.form.or_clauses) {
      state.rewind(s);
      state.nodes_used = 1;
      // start with all-negative clauses
      bool ok = 1;
      for(size_t i=cla.atom_count(); i--;) ok &= !cla.atom(i).sign();
      if(!ok) continue;

      COUNTER("strong");
      cla = cla.shift(state.val.size());
      state.val.resize(cla.var_count());
      state.clauses_used += cla;
      auto f2 = frames.tail();
      for(size_t i=0; i<cla.atom_count(); i++) {
        COUNTER("strong : atom");
        WeakFrame::Builder b;
        b->nodes_limit = f->nodes_limit;
        b->branch = Branch(cla.atom(i));
        f2 += Frame(b.build());
      }
      alts(Cont{f2});
    }
  }
  
  struct _WeakFrame { size_t nodes_limit; Branch branch; };
  using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

  template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.head())); 
    auto s = state.snapshot();
    if(state.nodes_used<f->nodes_limit) {
      SCOPE("expand");
      for(auto cla : state.form.or_clauses) {
        for(size_t i=0; i<cla.atom_count(); i++) {
          state.rewind(s);
          state.nodes_used++;
          cla = cla.shift(state.val.size());
          // do not use f->cla from now on
          state.val.resize(cla.var_count());
          COUNTER("strong : opposite");
          if(!state.val.opposite(f->branch.head(),cla.atom(i))) continue;
          state.clauses_used = cla + state.clauses_used;
          size_t branch_count = cla.atom_count()-1;
          if(!branch_count){ alts(Cont{frames.tail()}); continue; }
          auto f2 = frames.tail();
          for(size_t j=0; j<cla.atom_count(); j++) if(i!=j){
            COUNTER("strong : atom");
            WeakFrame::Builder b;
            b->nodes_limit = f->nodes_limit;
            b->branch = cla.atom(j) + f->branch;
            f2 += Frame(b.build());
          }
          alts(Cont{f2});
        }
      }
      state.rewind(s);
    }
    COUNTER("weak : unify loop");
    for(auto b2 = f->branch.tail(); !b2.empty(); b2 = b2.tail()) {
      state.rewind(s);
      COUNTER("weak : opposite");
      if(state.val.opposite(f->branch.head(),b2.head())) {
        alts(Cont{frames.tail()}); 
      }
    }
  }
};

ptr<Proof> prove(OrForm form, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(form);
  Cont::StartFrame::Builder b;
  b->nodes_limit = limit;
  Cont c{List<Cont::Frame>(Cont::Frame(b.build()))};
  struct Alt { Cont cont; typename SearchState::Snapshot snapshot; };
  List<Alt> alts({c,s.snapshot()});
  for(size_t steps = 0; !alts.empty(); steps++) {
    COUNTER("step");
    DEBUG if(steps%1000==0) info("steps = %",steps);
    auto a = alts.head(); alts = alts.tail();
    s.rewind(a.snapshot);
    if(a.cont.done()) return s.get_proof();
    a.cont.run(s,
      [&alts,&s](Cont cont){ alts += Alt{cont,s.snapshot()}; }
    );
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
    for(auto m : {"expand","weak : opposite","strong : opposite", "weak : unify loop", "strong : atom", "opposite: ok", "step"}) {
      std::cerr << util::fmt("%[%]: %\n",m,i,profile.scopes[m].count);
    }
  }
  DEBUG info("FAILURE");
  return 0;
}

#endif  // TABLEAU_H_
