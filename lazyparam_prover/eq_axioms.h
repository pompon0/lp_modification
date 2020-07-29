#ifndef EQ_AXIOMS_H_
#define EQ_AXIOMS_H_

#include <map>
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/derived.h"

namespace tableau {

DerAndClause neg_refl_axiom(memory::Alloc &A) {
  Term x(Var(A,0));
  return DerAndClause(A,0,AndClause::make(A,Atom::eq(A,false,x,x)));
}

DerAndClause neg_symm_axiom(memory::Alloc &A) {
  Term x(Var(A,0));
  Term y(Var(A,1));
  return DerAndClause(A,1,AndClause::make(A,
    Atom::eq(A,true,x,y),
    Atom::eq(A,false,y,x)
  ));
}

DerAndClause neg_trans_axiom(memory::Alloc &A) {
  Term x(Var(A,0));
  Term y(Var(A,1));
  Term z(Var(A,2));
  return DerAndClause(A,3,AndClause::make(A,
    Atom::eq(A,true,x,y),
    Atom::eq(A,true,y,z),
    Atom::eq(A,false,x,z)
  ));
}


DerAndClause neg_cong_pred_axiom(memory::Alloc &A, u64 pred_name, u64 arg_count) {
  Atom::Builder lb(A,true,pred_name,arg_count,false);
  Atom::Builder rb(A,false,pred_name,arg_count,false);
  AndClause::Builder cb(A,arg_count+2);
  for(size_t i=0; i<arg_count; ++i) {
    Term la(Var(A,2*i));
    Term ra(Var(A,2*i+1));
    cb.set_atom(i,Atom::eq(A,true,la,ra));
    lb.set_arg(i,la);
    rb.set_arg(i,ra);
  }
  cb.set_atom(arg_count,lb.build()); 
  cb.set_atom(arg_count+1,rb.build());
  return DerAndClause(A,3,cb.build());
}

DerAndClause neg_cong_fun_axiom(memory::Alloc &A, u64 fun_name, u64 arg_count) {
  Fun::Builder lb(A,fun_name,arg_count);
  Fun::Builder rb(A,fun_name,arg_count);
  AndClause::Builder cb(A,arg_count+1);
  for(size_t i=0; i<arg_count; ++i) {
    Term la(Var(A,2*i));
    Term ra(Var(A,2*i+1));
    cb.set_atom(i,Atom::eq(A,true,la,ra));
    lb.set_arg(i,la);
    rb.set_arg(i,ra);
  }
  cb.set_atom(arg_count,Atom::eq(A,false,Term(lb.build()),Term(rb.build()))); 
  return DerAndClause(A,3,cb.build());
}

struct ArityCtx {
  ArityCtx(){ pred_arity[Atom::EQ] = 2; }

  std::map<u64,size_t> pred_count;
  std::map<u64,u64> fun_arity;
  std::map<u64,u64> pred_arity;

  void traverse(Term t) {
    switch(t.type()) {
      case Term::VAR: return;
      case Term::FUN: {
        Fun f(t);
        if(fun_arity.count(f.fun()) && fun_arity[f.fun()]!=f.arg_count())
          error("arity mismatch for f% : got % and %",f.fun(),fun_arity[f.fun()],f.arg_count());
        fun_arity[f.fun()] = f.arg_count();
        for(size_t i=f.arg_count(); i--;) traverse(f.arg(i));
        return;
      }
    }
    error("unexpected t.type() = %",t.type());
  }

  void traverse(Atom a) {
    if(!pred_count.count(a.pred())) pred_count[a.pred()] = 0;
    pred_count[a.pred()]++;
    if(pred_arity.count(a.pred()) && pred_arity[a.pred()]!=a.arg_count())
      error("arity mismatch for p%, got % and %",a.pred(),pred_arity[a.pred()],a.arg_count());
    pred_arity[a.pred()] = a.arg_count();
    for(size_t i=a.arg_count(); i--;) traverse(a.arg(i));
    return;
  }

  void traverse(const AndClause &c) { for(size_t i=c.atom_count(); i--;) traverse(c.atom(i)); }
  void traverse(const DerAndClause &c) {
    traverse(c.derived());
    for(size_t i=c.source_count(); i--;) traverse(c.source(i));
  }
  void traverse(const OrForm &f) { for(const auto &c : f.and_clauses) traverse(c); }
};

bool has_equality(OrForm f) {
  ArityCtx ctx; ctx.traverse(f);
  return ctx.pred_count[Atom::EQ]>0;
}

// every (a!=a /\ ...) is subsumed by (a!=a)
OrForm add_refl_constraints(memory::Alloc &A, OrForm f) {
  for(auto &c : f.and_clauses) {
    auto b = c.to_builder(A);
    auto d = c.derived();
    for(size_t i=0; i<d.atom_count(); ++i) {
      auto a = d.atom(i);
      if(a.pred()==Atom::EQ && !a.sign()) {
        b.constraints.push_back(OrderAtom(A,OrderAtom::NE,a.arg(0),a.arg(1)));
      }
    }
    c = b.build();
  }
  return f;
}


OrForm append_eq_axioms(memory::Alloc &A, OrForm _f) {
  // 383/2003 don't use equality axioms (my CNF)
  // 516/2003 use reflexivity only (my CNF)
  // 549/2003 use refl + symm only (my CNF)
  // 740/2003 use refl + symm + mono (my CNF)
  // 640/2003 use refl + symm + trans (my CNF)
  OrForm f(add_refl_constraints(A,_f));
  ArityCtx ctx; ctx.traverse(f);
  f.and_clauses.push_back(neg_refl_axiom(A));
  f.and_clauses.push_back(neg_symm_axiom(A));
  f.and_clauses.push_back(neg_trans_axiom(A));
  for(auto [pred,arity] : ctx.pred_arity) if(pred!=Atom::EQ && arity) f.and_clauses.push_back(neg_cong_pred_axiom(A,pred,arity));
  for(auto [fun,arity] : ctx.fun_arity) if(arity) f.and_clauses.push_back(neg_cong_fun_axiom(A,fun,arity));
  //info("f + axioms = \n%",show(OrForm(f)));
  return f;
}


/////////////////////////////////////////////////////////////////////////////

struct FlatClauseBuilder {
  memory::Alloc *A;
  size_t var_count;
  vec<Atom> atoms;
  vec<AndClause> source_clauses;
  size_t cost;

  FlatClauseBuilder(memory::Alloc &_A, DerAndClause cla) : A(&_A) {
    cost = cla.cost();
    var_count = cla.derived().var_range().end;
    for(size_t i=0; i<cla.source_count(); i++) {
      source_clauses.push_back(cla.source(i));
    }
    auto d = cla.derived();
    for(size_t i=d.atom_count(); i--;) flatten_Atom(d.atom(i));
  }

  Var introduce_var(Term t) {
    switch(t.type()) {
      case Term::VAR: {
        source_clauses.push_back(AndClause::make(*A,Atom::eq(*A,false,t,t)));
        return Var(t);
      }
      case Term::FUN: {
        Fun fa(t);
        Fun fv = flatten_Term(fa);
        // (f(a1..an)!=f(v1..vn)) (f(a1..an)=f(v1..vn) /\ f(v1..vn)=x /\ f(a1..an)!=x)
        // -> (f(v1..vn)=x /\ f(a1..an)!=x)
        Var x(*A,var_count++);
        auto fa_fv = Atom::eq(*A,true,Term(fa),Term(fv));
        auto fv_x = Atom::eq(*A,true,Term(fv),Term(x));
        auto fa_x = Atom::eq(*A,true,Term(fa),Term(x));
        source_clauses.push_back(AndClause::make(*A,fa_fv,fv_x,fa_x.neg()));
        atoms.push_back(fv_x);
        return x;
      }
      default: error("t.type() = %",t.type());
    }
  }

  Fun flatten_Term(Fun fa) {
    // (a1!=v1)..(an!=vn)
    // (a1=v1 /\../\ an=vn /\ f(a1..an)!=f(v1..vn))
    // -> (f(a1..an)!=f(v1..vn))
    size_t n = fa.arg_count();
    AndClause::Builder b(*A,n+1);
    Fun::Builder fb(*A,fa.fun(),fa.arg_count());
    for(size_t i=0; i<n; ++i){
      Var vi = introduce_var(fa.arg(i));
      b.set_atom(i,Atom::eq(*A,true,fa.arg(i),Term(vi)));
      fb.set_arg(i,Term(vi));
    }
    auto fv = fb.build();
    b.set_atom(n,Atom::eq(*A,false,Term(fa),Term(fv)));
    source_clauses.push_back(b.build());
    return fv;
  }

  void flatten_Atom(Atom a) {
    if(a.pred()==Atom::EQ) {
      auto l = a.arg(0);
      auto r = a.arg(1);
      if(l.type()==Term::FUN) {
        // (..l=r) (l=l2 /\ l2=r /\ l!=r)
        Term l2(flatten_Term(Fun(l)));
        source_clauses.push_back(
          AndClause::make(*A,Atom::eq(*A,true,l,l2),Atom::eq(*A,a.sign(),l2,r),Atom::eq(*A,!a.sign(),l,r)));
        l = l2;
      }
      if(r.type()==Term::FUN) {
        // (..l=r) (r=r2 /\ l=r2 /\ l!=r)
        Term r2(flatten_Term(Fun(r)));
        source_clauses.push_back(
          AndClause::make(*A,Atom::eq(*A,true,r,r2),Atom::eq(*A,a.sign(),l,r2),Atom::eq(*A,!a.sign(),l,r)));
        r = r2;
      }
      atoms.push_back(Atom::eq(*A,a.sign(),l,r));
    } else {
      Atom::Builder b(*A,a.sign(),a.pred(),a.arg_count(),a.strong_only());
      // (... p(a1..an)) (a1=v1 /\../\ an=vn /\ !p(a1..an) /\ p(v1..vn)) -> (... p(v1..vn))
      size_t n = a.arg_count();
      AndClause::Builder cb(*A,n+2);
      for(size_t i=0; i<a.arg_count(); ++i) {
        Var v = introduce_var(a.arg(i));
        cb.set_atom(i,Atom::eq(*A,true,a.arg(i),Term(v)));
        b.set_arg(i,Term(v));
      }
      auto pv = b.build();
      cb.set_atom(n,a.neg());
      cb.set_atom(n+1,pv);
      source_clauses.push_back(cb.build());
      atoms.push_back(pv);
    }
  }

  DerAndClause build() const {
    DerAndClause::Builder b(*A);
    b.cost = cost;
    AndClause::Builder cb(*A,atoms.size());
    for(size_t i=atoms.size(); i--;) cb.set_atom(i,atoms[i]);
    b.derived = cb.build();
    b.sources = source_clauses;
    return b.build();
  }
};

inline OrForm flatten_OrForm(memory::Alloc &A, OrForm f) {
  OrForm f2;
  for(auto cla : f.and_clauses) f2.and_clauses.push_back(FlatClauseBuilder(A,cla).build());
  return f2;
}

OrForm reduce_monotonicity_and_append_eq_axioms(memory::Alloc &A, OrForm _f) {
  OrForm f(flatten_OrForm(A,_f));
  f.and_clauses.push_back(neg_refl_axiom(A));
  f.and_clauses.push_back(neg_symm_axiom(A));
  f.and_clauses.push_back(neg_trans_axiom(A));
  //info("f = \n%",show(_f));
  //info("m(f) + axioms = \n%",show(OrForm(f)));
  return OrForm(f);
}

OrForm append_restricted_transitivity_axioms(memory::Alloc &A, OrForm f) {
  Term a(Var(A,0));
  Term b(Var(A,1));
  Term c(Var(A,2));

  // -[a=b] symm(a,b)
  {
    DerAndClause::Builder symm1(A);
    symm1.cost = 0;
    symm1.derived = AndClause::make(A,
      Atom(A,true,Atom::EQ_TRANS_POS,{a,b}),
      Atom(A,false,Atom::EQ_SYMM,{a,b})
    );
    f.and_clauses.push_back(symm1.build());
  }
  // -[b=a] symm(a,b)
  {
    DerAndClause::Builder symm2(A);
    symm2.cost = 0;
    symm2.derived = AndClause::make(A,
      Atom(A,true,Atom::EQ_TRANS_POS,{b,a}),
      Atom(A,false,Atom::EQ_SYMM,{a,b})
    );
    symm2.sources.push_back(AndClause::make(A,
      Atom(A,true,Atom::EQ,{b,a}),
      Atom(A,false,Atom::EQ,{a,b})
    ));
    f.and_clauses.push_back(symm2.build());
  }
  // -symm(a,b) b/=c a=c
  {
    DerAndClause::Builder trans_pos(A);
    trans_pos.cost = 1;
    trans_pos.derived = AndClause::make(A,
      Atom(A,true,Atom::EQ_SYMM,{a,b}),
      Atom(A,true,Atom::EQ,{b,c}),
      Atom(A,false,Atom::EQ,{a,c})
    );
    trans_pos.sources.push_back(AndClause::make(A,
      Atom(A,true,Atom::EQ,{a,b}),
      Atom(A,true,Atom::EQ,{b,c}),
      Atom(A,false,Atom::EQ,{a,c})
    ));
    f.and_clauses.push_back(trans_pos.build());
  }
  // {a=b} a/=c b/=c
  {
    DerAndClause::Builder trans_neg(A);
    trans_neg.cost = 0;
    trans_neg.derived = AndClause::make(A,
      Atom(A,false,Atom::EQ_TRANS_NEG,{a,b}),
      Atom(A,true,Atom::EQ,{b,c}),
      Atom(A,true,Atom::EQ,{a,c})
    );
    trans_neg.sources.push_back(AndClause::make(A,
      Atom(A,false,Atom::EQ,{a,b}),
      Atom(A,true,Atom::EQ,{b,c}),
      Atom(A,true,Atom::EQ,{a,c})
    ));
    f.and_clauses.push_back(trans_neg.build());
  }
  return f;
}


OrForm append_eq_axioms_with_restricted_transitivity(memory::Alloc &A, OrForm f) {
  ArityCtx ctx; ctx.traverse(f);
  for(auto [pred,arity] : ctx.pred_arity) if(pred!=Atom::EQ && arity) f.and_clauses.push_back(neg_cong_pred_axiom(A,pred,arity));
  for(auto [fun,arity] : ctx.fun_arity) if(arity) f.and_clauses.push_back(neg_cong_fun_axiom(A,fun,arity));
  for(auto &dc : f.and_clauses) {
    auto c = dc.derived();
    AndClause::Builder b(A,c.atom_count());
    for(size_t i=c.atom_count(); i--;) {
      Atom a = c.atom(i);
      if(a.pred()==Atom::EQ) {
        Atom::Builder ab(A,a.sign(),!a.sign()?Atom::EQ_TRANS_POS:Atom::EQ_TRANS_NEG,a.arg_count(),a.strong_only());
        for(size_t j=a.arg_count(); j--;) ab.set_arg(j,a.arg(j));
        b.set_atom(i,ab.build());
      } else b.set_atom(i,a);
    }
    auto db = dc.to_builder(A);
    db.derived = b.build();
    dc = db.build();
  }
  f.and_clauses.push_back(neg_refl_axiom(A));
  f = append_restricted_transitivity_axioms(A,f);
  //info("f + axioms = \n%",show(OrForm(f)));
  return OrForm(f);
}


/////////////////////////////////////////////////////////////////////////////

struct Index {
  static size_t atom_hash(size_t pred, bool sign){ return (pred-Atom::PRED_MIN)<<1|sign; }
  static size_t atom_hash(Atom a) { return atom_hash(a.pred(),a.sign()); }
};

} // namespace tableau

#endif  // EQ_AXIOMS_H_
