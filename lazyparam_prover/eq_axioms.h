#ifndef EQ_AXIOMS_H_
#define EQ_AXIOMS_H_

#include <map>
#include "lazyparam_prover/util/log.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/pred_format.h"

namespace {

DerOrClause refl_axiom() {
  size_t var_count = 0;
  Term x(Var::make(var_count++));
  OrClause::Builder b(1,var_count);
  b.set_atom(0,Atom::eq(true,x,x));
  return DerOrClause(0,b.build());
}

DerOrClause symm_axiom() {
  size_t var_count = 0;
  Term x(Var::make(var_count++));
  Term y(Var::make(var_count++));
  OrClause::Builder b(2,var_count);
  b.set_atom(0,Atom::eq(false,x,y));
  b.set_atom(1,Atom::eq(true,y,x));
  return DerOrClause(1,b.build());
}

DerOrClause trans_axiom() {
  size_t var_count = 0;
  Term x(Var::make(var_count++));
  Term y(Var::make(var_count++));
  Term z(Var::make(var_count++));
  OrClause::Builder b(3,var_count);
  b.set_atom(0,Atom::eq(false,x,y));
  b.set_atom(1,Atom::eq(false,y,z));
  b.set_atom(2,Atom::eq(true,x,z));
  return DerOrClause(3,b.build());
}


DerOrClause cong_pred_axiom(u64 pred_name, u64 arg_count) {
  size_t var_count = 2*arg_count;
  Atom::Builder lb(false,pred_name,arg_count);
  Atom::Builder rb(true,pred_name,arg_count);
  OrClause::Builder cb(arg_count+2,var_count);
  for(size_t i=0; i<arg_count; ++i) {
    Term la(Var::make(2*i));
    Term ra(Var::make(2*i+1));
    cb.set_atom(i,Atom::eq(false,la,ra));
    lb.set_arg(i,la);
    rb.set_arg(i,ra);
  }
  cb.set_atom(arg_count,lb.build()); 
  cb.set_atom(arg_count+1,rb.build());
  return DerOrClause(3,cb.build());
}

DerOrClause cong_fun_axiom(u64 fun_name, u64 arg_count) {
  size_t var_count = 2*arg_count;
  Fun::Builder lb(fun_name,arg_count);
  Fun::Builder rb(fun_name,arg_count);
  OrClause::Builder cb(arg_count+1,var_count);
  for(size_t i=0; i<arg_count; ++i) {
    Term la(Var::make(2*i));
    Term ra(Var::make(2*i+1));
    cb.set_atom(i,Atom::eq(false,la,ra));
    lb.set_arg(i,la);
    rb.set_arg(i,ra);
  }
  cb.set_atom(arg_count,Atom::eq(true,Term(lb.build()),Term(rb.build()))); 
  return DerOrClause(3,cb.build());
}

struct ArityCtx {
  ArityCtx(){ pred_arity[Atom::EQ] = 2; }

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
    if(pred_arity.count(a.pred()) && pred_arity[a.pred()]!=a.arg_count())
      error("arity mismatch for p%, got % and %",a.pred(),pred_arity[a.pred()],a.arg_count());
    pred_arity[a.pred()] = a.arg_count();
    for(size_t i=a.arg_count(); i--;) traverse(a.arg(i));
    return;
  }

  void traverse(const OrClause &c) { for(size_t i=c.atom_count(); i--;) traverse(c.atom(i)); }
  void traverse(const DerOrClause &c) { traverse(c.derived()); for(auto cla : c.source()) traverse(cla); }
  void traverse(const NotAndForm &f) { for(const auto &c : f.or_clauses) traverse(c); }
};


OrForm append_eq_axioms(OrForm _f) {
  // 383/2003 don't use equality axioms (my CNF)
  // 516/2003 use reflexivity only (my CNF)
  // 549/2003 use refl + symm only (my CNF)
  // 740/2003 use refl + symm + mono (my CNF)
  // 640/2003 use refl + symm + trans (my CNF)
  NotAndForm f(_f);
  ArityCtx ctx; ctx.traverse(f);
  f.or_clauses.push_back(refl_axiom());
  f.or_clauses.push_back(symm_axiom());
  f.or_clauses.push_back(trans_axiom());
  //TODO: do not add axioms for funs and preds without args
  for(auto pa : ctx.pred_arity) if(pa.first!=Atom::EQ && pa.second) f.or_clauses.push_back(cong_pred_axiom(pa.first,pa.second));
  for(auto fa : ctx.fun_arity) if(fa.second) f.or_clauses.push_back(cong_fun_axiom(fa.first,fa.second));
  info("f + axioms = \n%",show(OrForm(f)));
  return OrForm(f);
}

}

/////////////////////////////////////////////////////////////////////////////

struct FlatClauseBuilder {
  size_t var_count;
  vec<Atom> atoms;
  vec<AndClause> source_clauses;
  size_t cost;

  FlatClauseBuilder(DerAndClause cla) {
    cost = cla.cost;
    var_count = cla.derived.var_count;
    source_clauses = cla.source;
    for(auto atom : cla.derived.atoms) flatten_Atom(atom);
  }

  Var introduce_var(Term t) {
    switch(t.type()) {
      case Term::VAR: {
        AndClause cla;
        cla.atoms = {Atom::eq(false,t,t)};
        cla.var_count = var_count;
        source_clauses.push_back(cla);
        return Var(t);
      }
      case Term::FUN: {
        Fun fa(t);
        Fun fv = flatten_Term(fa);
        // (f(a1..an)!=f(v1..vn)) (f(a1..an)=f(v1..vn) /\ f(v1..vn)=x /\ f(a1..an)!=x)
        // -> (f(v1..vn)=x /\ f(a1..an)!=x)
        Var x = Var::make(var_count++);
        auto fa_fv = Atom::eq(true,Term(fa),Term(fv));
        auto fv_x = Atom::eq(true,Term(fv),Term(x));
        auto fa_x = Atom::eq(true,Term(fa),Term(x));
        AndClause cla;
        cla.atoms = {fa_fv,fv_x,fa_x.neg()};
        cla.var_count = var_count;
        source_clauses.push_back(cla);
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
    AndClause cla;
    Fun::Builder fb(fa.fun(),fa.arg_count());
    for(size_t i=0; i<fa.arg_count(); ++i){
      Var vi = introduce_var(fa.arg(i));
      cla.atoms.push_back(Atom::eq(true,fa.arg(i),Term(vi)));
      fb.set_arg(i,Term(vi));
    }
    auto fv = fb.build();
    cla.atoms.push_back(Atom::eq(false,Term(fa),Term(fv)));
    source_clauses.push_back(cla);
    cla.var_count = var_count;
    return fv;
  }

  void flatten_Atom(Atom a) {
    if(a.pred()==Atom::EQ) {
      auto l = a.arg(0);
      auto r = a.arg(1);
      if(l.type()==Term::FUN) {
        // (..l=r) (l=l2 /\ l2=r /\ l!=r)
        Term l2(flatten_Term(Fun(l)));
        AndClause cla;
        cla.atoms = {Atom::eq(true,l,l2),Atom::eq(a.sign(),l2,r),Atom::eq(!a.sign(),l,r)};
        cla.var_count = var_count;
        source_clauses.push_back(cla);
        l = l2;
      }
      if(r.type()==Term::FUN) {
        // (..l=r) (r=r2 /\ l=r2 /\ l!=r)
        Term r2(flatten_Term(Fun(r)));
        AndClause cla;
        cla.atoms = {Atom::eq(true,r,r2),Atom::eq(a.sign(),l,r2),Atom::eq(!a.sign(),l,r)};
        cla.var_count = var_count;
        source_clauses.push_back(cla);
        r = r2;
      }
      atoms.push_back(Atom::eq(a.sign(),l,r));
    } else {
      Atom::Builder b(a.sign(),a.pred(),a.arg_count());
      // (... p(a1..an)) (a1=v1 /\../\ an=vn /\ !p(a1..an) /\ p(v1..vn)) -> (... p(v1..vn))
      AndClause cla;
      for(size_t i=0; i<a.arg_count(); ++i) {
        Var v = introduce_var(a.arg(i));
        cla.atoms.push_back(Atom::eq(true,a.arg(i),Term(v)));
        b.set_arg(i,Term(v));
      }
      auto pv = b.build();
      cla.atoms.push_back(a.neg());
      cla.atoms.push_back(pv);
      cla.var_count = var_count;
      source_clauses.push_back(cla);
      atoms.push_back(pv);
    }
  }

  DerAndClause build() const {
    AndClause cla(var_count);
    cla.atoms = atoms;
    DerAndClause dc;
    dc.derived = cla;
    dc.source = source_clauses;
    dc.cost = cost;
    return dc;
  }
};

namespace {

inline OrForm flatten_OrForm(OrForm f) {
  OrForm f2;
  for(auto cla : f.and_clauses) f2.and_clauses.push_back(FlatClauseBuilder(cla).build());
  return f2;
}

OrForm reduce_monotonicity_and_append_eq_axioms(OrForm _f) {
  NotAndForm f(flatten_OrForm(_f));
  f.or_clauses.push_back(refl_axiom());
  f.or_clauses.push_back(symm_axiom());
  f.or_clauses.push_back(trans_axiom());
  info("f = \n%",show(_f));
  info("m(f) + axioms = \n%",show(OrForm(f)));
  return OrForm(f);
}
}

/////////////////////////////////////////////////////////////////////////////

struct Index {
  struct OrClauseWithAtom { size_t i; DerOrClause cla; };
  static size_t atom_hash(Atom a) { return (a.pred()-Atom::PRED_MIN)<<1|a.sign(); }
private:
  vec<OrClauseWithAtom> empty;
  vec<vec<vec<OrClauseWithAtom>>> map; // atom -> cost -> OrClauseWithAtom
public:
  Index(const NotAndForm &f) { FRAME("Index");
    for(auto cla : f.or_clauses) {
      DEBUG info("cla.derived.atom_count() = %",cla.derived().atom_count());
      for(size_t i=0; i<cla.derived().atom_count(); ++i) {
        DEBUG info("Index i=%",i);
        auto h = atom_hash(cla.derived().atom(i));
        DEBUG info("h = %",h);
        if(map.size()<=h) map.resize(h+1,{{}});
        DEBUG info("cla.cost() = %",cla.cost());
        if(map[h].size()<=cla.cost()) map[h].resize(cla.cost()+1,map[h].back());
        for(size_t cc=cla.cost(); cc<map[h].size(); ++cc) map[h][cc].push_back({i,cla});
      }
    }
  }

  // find all atoms with same pred and opposite sign
  const vec<OrClauseWithAtom>& operator()(Atom a, size_t max_cost) const {
    auto h = atom_hash(a)^1;
    return h>=map.size() ? empty : max_cost<map[h].size() ? map[h][max_cost] : map[h].back();
  }
};

#endif  // EQ_AXIOMS_H_
