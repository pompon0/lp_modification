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
  SearchState(OrForm _form) : form(_form), cla_index(form) {}
 
  NotAndForm form;
  const Index cla_index;
  Valuation val;

  ptr<Proof> get_proof(List<DerOrClause> clauses_used) {
    ptr<Proof> proof(new Proof);
    for(auto l=clauses_used; !l.empty(); l = l.tail()) {
      for(auto cla : l.head().source()) {
        proof->source.push_back(ground(val.eval(cla)).neg());
      }
    }
    return proof;
  }

  struct Snapshot {
    Valuation::Snapshot val;
    ::Snapshot stack;
  };

  void rewind(Snapshot s) {
    val.rewind(s.val);
    stack = s.stack;
  }

  Snapshot snapshot(){
    return {val.snapshot(),stack};
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
    enum Type { START, STRONG, WEAK, WEAK_UNIFY };
    Type type() const { return Type(*LType::at(ptr)); }
  private:
    using LType = Lens<size_t,0>;
    enum { SIZE = LType::END };
    uint8_t *ptr;
    explicit Frame(uint8_t *_ptr) : ptr(_ptr) {}

    friend Variant<Frame,START,_StartFrame>;
    friend Variant<Frame,STRONG,_StrongFrame>;
    friend Variant<Frame,WEAK,_WeakFrame>;
    friend Variant<Frame,WEAK_UNIFY,_WeakUnifyFrame>;
  };
  
  size_t nodes_used;
  List<DerOrClause> clauses_used;
  List<Frame> frames;
  bool done(){ return frames.empty(); }

  template<typename Alts> void run(State &state, Alts alts) const { FRAME("run");
    DEBUG if(frames.empty()) error("frames.empty()");
    auto f = frames.head();
    switch(f.type()) {
      case Frame::START: start(state,StartFrame(f),alts); break;
      case Frame::STRONG: strong(state,StrongFrame(f),alts); break;
      case Frame::WEAK: weak(state,WeakFrame(f),alts); break;
      case Frame::WEAK_UNIFY: weak_unify(state,WeakUnifyFrame(f),alts); break;
      default: error("f.type() = %",f.type());
    }
  }

  struct _StartFrame { size_t nodes_limit; };
  using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

  template<typename Alts> void start(State &state, StartFrame f, Alts alts) const { FRAME("start");
    for(auto dcla : state.form.or_clauses) {
      dcla = dcla.shift(state.val.size());
      OrClause cla = dcla.derived();
      // start will all-negative clauses
      bool ok = 1;
      for(size_t i=cla.atom_count(); i--;) ok &= !cla.atom(i).sign();
      if(!ok) continue;
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = Branch();
      b->dcla = dcla;
      b->strong_id = -1;
      alts(Cont{dcla.cost() + nodes_used, dcla + clauses_used, List<Frame>(Frame(b.build()))});
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
    auto cla = f->dcla.derived();
    if(state.val.size()<cla.var_count()) state.val.resize(cla.var_count());
    if(f->strong_id>=0 && !state.val.opposite(f->branch.true_.head(),cla.atom(f->strong_id))) return;
    List<Atom> false_ = f->branch.false_; 
    List<Frame> wf = frames.tail();
    for(ssize_t i=cla.atom_count(); i--;) if(i!=f->strong_id) {
      Atom a = cla.atom(i);
      bool a_is_false = 0;
      bool a_is_true = 0;
      for(auto ft = f->branch.true_; !ft.empty(); ft = ft.tail()) if(state.val.equal_mod_sign(ft.head(),a)) {
        if(ft.head().sign()==a.sign()) return; // the new clause is disjoint with the target superspace (assumed to be nonempty for every groud clause considered).
        else a_is_false = 1;
      }
      for(auto ft = false_; !ft.empty(); ft = ft.tail()) if(state.val.equal_mod_sign(ft.head(),a)) {
        (ft.head().sign()==a.sign() ? a_is_false : a_is_true) = 1;
      }
      if(a_is_false) {
        if(a_is_true) break; // all the remaining branches have empty target subspace
        continue; // this particular branch has empty target subspace
      }
      WeakFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = Branch{cla.atom(i) + f->branch.true_, false_};
      wf += Frame(b.build());
      false_ += cla.atom(i);
    }
    alts(Cont{nodes_used,clauses_used,wf});
  }

  struct _WeakFrame { size_t nodes_limit; Branch branch; };
  using WeakFrame = Variant<Frame,Frame::WEAK,_WeakFrame>;

  template<typename Alts> void weak(State &state, WeakFrame f, Alts alts) const { FRAME("weak(%)",show(f->branch.true_.head())); 
    size_t budget = f->nodes_limit - nodes_used;
    COUNTER("expand");
    auto atom_hash = Index::atom_hash(f->branch.true_.head())^1;
    for(auto ut = clauses_used; !ut.empty(); ut = ut.tail()) {
      const OrClause &c = ut.head().derived();
      for(size_t i=0; i<c.atom_count(); ++i) {
        if(atom_hash==Index::atom_hash(c.atom(i))) {
          StrongFrame::Builder b;
          b->nodes_limit = f->nodes_limit;
          b->branch = f->branch;
          b->dcla = ut.head();
          b->strong_id = i;
          alts(Cont{nodes_used, clauses_used, Frame(b.build()) + frames.tail()});
        }
      }
    }
    for(auto ca : state.cla_index(f->branch.true_.head(),budget)) {
      StrongFrame::Builder b;
      b->nodes_limit = f->nodes_limit;
      b->branch = f->branch;
      b->dcla = ca.cla.shift(state.val.size());
      b->strong_id = ca.i;
      alts(Cont{nodes_used + b->dcla.cost(), b->dcla + clauses_used, Frame(b.build()) + frames.tail()});
    }
    for(auto b2 = f->branch.true_.tail(); !b2.empty(); b2 = b2.tail()) {
      if(atom_hash!=Index::atom_hash(b2.head())) continue;
      WeakUnifyFrame::Builder b;
      b->a1 = f->branch.true_.head();
      b->a2 = b2.head();
      alts(Cont{nodes_used, clauses_used, Frame(b.build())+frames.tail()});
    }
  }
  
  struct _WeakUnifyFrame { Atom a1,a2; };
  using WeakUnifyFrame = Variant<Frame,Frame::WEAK_UNIFY,_WeakUnifyFrame>;
  template<typename Alts> void weak_unify(State &state, WeakUnifyFrame f, Alts &alts) const { FRAME("weak_unify");
    if(state.val.opposite(f->a1,f->a2)) alts(Cont{nodes_used,clauses_used,frames.tail()}); 
  }  
};

ptr<Proof> prove(OrForm form, size_t limit) { FRAME("prove()");
  SCOPE("prove");
  SearchState s(form);
  Cont::StartFrame::Builder b;
  b->nodes_limit = limit;
  Maybe<Cont> cont = alt::search(s,Cont{0,List<DerOrClause>(),List<Cont::Frame>(Cont::Frame(b.build()))});
  if(cont) return s.get_proof(cont.get().clauses_used);
  return 0;
}

ptr<Proof> prove_loop(OrForm form, size_t limit) { FRAME("prove_loop()");
  SCOPE("prove_loop");
  //form = reduce_monotonicity_and_append_eq_axioms(form);
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
