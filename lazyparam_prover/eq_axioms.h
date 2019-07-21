#ifndef EQ_AXIOMS_H_
#define EQ_AXIOMS_H_

#include <map>
#include "lazyparam_prover/util/log.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/pred_format.h"

namespace {

OrClause refl_axiom() {
  size_t var_count = 0;
  Term x(Var::make(var_count++));
  OrClause::Builder b(1,var_count);
  b.set_atom(0,Atom::eq(true,x,x));
  return b.build();
}

OrClause symm_axiom() {
  size_t var_count = 0;
  Term x(Var::make(var_count++));
  Term y(Var::make(var_count++));
  OrClause::Builder b(2,var_count);
  b.set_atom(0,Atom::eq(false,x,y));
  b.set_atom(1,Atom::eq(true,y,x));
  return b.build();
}

OrClause trans_axiom() {
  size_t var_count = 0;
  Term x(Var::make(var_count++));
  Term y(Var::make(var_count++));
  Term z(Var::make(var_count++));
  OrClause::Builder b(3,var_count);
  b.set_atom(0,Atom::eq(false,x,y));
  b.set_atom(1,Atom::eq(false,y,z));
  b.set_atom(2,Atom::eq(true,x,z));
  return b.build();
}


OrClause cong_pred_axiom(u64 pred_name, u64 arg_count) {
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
  return cb.build();
}

OrClause cong_fun_axiom(u64 fun_name, u64 arg_count) {
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
  return cb.build();
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
  void traverse(const NotAndForm &f) { for(const auto &c : f.or_clauses) traverse(c); }
};


OrForm append_eq_axioms(OrForm _f) {
  NotAndForm f(_f);
  ArityCtx ctx; ctx.traverse(f);
  f.or_clauses.push_back(refl_axiom());
  f.or_clauses.push_back(symm_axiom());
  f.or_clauses.push_back(trans_axiom());
  //TODO: do not add axioms for funs and pres without args
  for(auto pa : ctx.pred_arity) if(pa.first!=Atom::EQ && pa.second) f.or_clauses.push_back(cong_pred_axiom(pa.first,pa.second));
  for(auto fa : ctx.fun_arity) if(fa.second) f.or_clauses.push_back(cong_fun_axiom(fa.first,fa.second));
  info("f + axioms = \n%",show(OrForm(f)));
  return OrForm(f);
}

}

#endif  // EQ_AXIOMS_H_
