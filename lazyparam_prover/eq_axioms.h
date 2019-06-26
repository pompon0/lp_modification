#ifndef EQ_AXIOMS_H_
#define EQ_AXIOMS_H_

#include <map>
#include "lazyparam_prover/util/log.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/pred_format.h"

namespace {

OrClause refl_axiom() {
  OrClause c;
  Term x(Var::make(c.var_count++));
  c.atoms = {Atom::eq(true,x,x)};
  return c;
}

OrClause symm_axiom() {
  OrClause c;
  Term x(Var::make(c.var_count++));
  Term y(Var::make(c.var_count++));
  c.atoms = {Atom::eq(false,x,y),Atom::eq(true,y,x)};
  return c;
}

OrClause trans_axiom() {
  OrClause c;
  Term x(Var::make(c.var_count++));
  Term y(Var::make(c.var_count++));
  Term z(Var::make(c.var_count++));
  c.atoms = {Atom::eq(false,x,y),Atom::eq(false,y,z),Atom::eq(true,x,z)};
  return c;
}


OrClause cong_pred_axiom(u64 pred_name, u64 arg_count) {
  OrClause c;
  Atom::Builder lb(false,pred_name,arg_count,0);
  Atom::Builder rb(true,pred_name,arg_count,0);
  for(size_t i=0; i<arg_count; ++i) {
    Term la(Var::make(c.var_count++));
    Term ra(Var::make(c.var_count++));
    c.atoms.push_back(Atom::eq(false,la,ra));
    lb.set_arg(i,la);
    rb.set_arg(i,ra);
  }
  c.atoms.push_back(lb.build()); 
  c.atoms.push_back(rb.build());
  return c;
}

OrClause cong_fun_axiom(u64 fun_name, u64 arg_count) {
  OrClause c;
  Fun::Builder lb(fun_name,arg_count);
  Fun::Builder rb(fun_name,arg_count);
  for(size_t i=0; i<arg_count; ++i) {
    Term la(Var::make(c.var_count++));
    Term ra(Var::make(c.var_count++));
    c.atoms.push_back(Atom::eq(false,la,ra));
    lb.set_arg(i,la);
    rb.set_arg(i,ra);
  }
  c.atoms.push_back(Atom::eq(true,Term(lb.build()),Term(rb.build()))); 
  return c;
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

  void traverse(const OrClause &c) { for(auto a : c.atoms) traverse(a); }
  void traverse(const NotAndForm &f) { for(const auto &c : f.or_clauses) traverse(c); }
};


OrForm append_eq_axioms(OrForm _f) {
  NotAndForm f(_f);
  ArityCtx ctx; ctx.traverse(f);
  f.or_clauses.push_back(refl_axiom());
  f.or_clauses.push_back(symm_axiom());
  f.or_clauses.push_back(trans_axiom());
  for(auto pa : ctx.pred_arity) if(pa.first!=Atom::EQ && pa.second) f.or_clauses.push_back(cong_pred_axiom(pa.first,pa.second));
  for(auto fa : ctx.fun_arity) if(fa.second) f.or_clauses.push_back(cong_fun_axiom(fa.first,fa.second));
  info("f + axioms = \n%",show(OrForm(f)));
  return OrForm(f);
}

}

#endif  // EQ_AXIOMS_H_
