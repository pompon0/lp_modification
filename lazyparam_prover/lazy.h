#ifndef LAZY_H_
#define LAZY_H_

// define signature function
//   s(f(a,b)) := f(s(a),s(b)
//   s(x) := v
// do initial flattening
//   f(a,b) = g(c)  ==>  (f(a,b),F) [=] x /\ (g(c),F) [=] x [f(a,b)>=x, g(c)>=x]
//   f(a,b) != g(c)  ==>  (a,s(a)) -> x /\ (b,s(b)) -> y /\ (f(x,y),F) != (g(z),F) /\ (c,s(c)) -> z [a>=x,b>=y,c>=z]
// generate signature aware mono axioms
//   [3] (f(y,z),f(u1,u2)) !-> x /\ (f(y',z'),F) [=] x /\ (y,u1) -> y' /\ (z,u2) -> z' [f(y,z)>f(y',z')]
//   [0] (f(y,z),f(u1,u2)) !-> x /\ (f(y,z),F) [=] x [f(y,z)>x]
//   [0] (x,y) !-> x [you can assume that sides are different if this axiom doesn't match]
// generate signature aware trans axioms
//   (x,F) = (y,ys) /\ (z,zs) = (x,F) /\ (y,ys) != (z,zs) [x>y,x>z]
//     we can additionally always enforce x>y in (x,xs) = (y,ys), by applying appropriate symmetry axiom between every 2 transitivity axioms
//   (x,F) ![=] z /\ (x,xs) = (y,ys) /\ (y,ys) [=] z [x>y]
//   (x,xs) ![=] x
// generate signature aware symmetry axiom
//   (x,xs) = (y,ys) /\ (y,ys) != (x,xs) [y>x]

namespace lazy {

struct ClauseConverter {
  DerAndClause cla; // TODO: source

  Term signature(Term t) const {
    switch(t.type()) {
      case Term::VAR: {
        return Fun::Builder(VAR_WRAP,0).build();
      }
      case Term::FUN: {
        Fun f(t);
        size_t ac = f.arg_count();
        Fun::Builder b(f.fun(),ac);
        for(size_t i=0; i<ac; ++i) b.set_arg(i,signature(f.arg(i)));
        return b.build();
      }
    }
  }
  
  Term flatten_once(Term t) {
    switch(t.type()) {
      case Term::VAR: return t;
      case Term::FUN: {
        Fun f(t);
        size_t ac = f.arg_count();
        Fun::Builer b(f.fun(),ac);
        for(size_t i=0; i<ac; ++i) switch(f.arg(i).type()) {
          case Term::VAR: b.set_arg(i,f.arg(i)); break;
          case Term::FUN: {
            Term u(Var::make(cla.var_count++));
            b.set_arg(i,u);
            Atom::Builder ab(true,Atom::MONO_RED,3);
            ab.set_arg(0,f.arg(i));
            ab.set_arg(1,signature(f.arg(i));
            ab.set_arg(2,u);
            cla.derived.atoms.push_back(ab.build());
            break;
          }
        }
        return b.build();
      }
    }
  }

  Atom flatten_once(Atom a) const {
    auto vw = Fun::Builder(Atom::VAR_WRAP,0).build();
    auto fw = Fun::Builder(Atom::FUN_WRAP,0).build();
    if(a.pred()==Atom::EQ) {
      auto l = flatten_once(a.arg(0));
      auto r = flatten_once(a.arg(1));
      if(a.sign()) {
        if(r.type()==Term::FUN) switch(l.type()) {
          case Term::VAR: std::swap(l,r); break;
          case Term::FUN: {           
            Term u(Var::make(cla.var_count++));
            Atom::Builder rb(true,Atom::TRANS_TARGET,3);
            lb.set_arg(0,r);
            lb.set_arg(1,fw);
            lb.set_arg(2,u);
            cla.derived.atoms.push_back(rb.build());
            r = u;
          }
        }
        Atom::Builder lb(true,Atom::TRANS_TARGET,3);
        lb.set_arg(0,l);
        lb.set_arg(1,l.type()==Term::FUN ? fw : vw);
        lb.set_arg(2,r);
        cla.derived.atoms.push_back(lb.build());
      } else {
        Atom::Builder trb(true,Atom::TRANS_RED,4);
        trb.set_arg(0,l);
        trb.set_arg(1,l.type()==Term::FUN ? fw : vw);
        trb.set_arg(2,r);
        trb.set_arg(3,r.type()==Term::FUN ? fw : vw);
        cla.derived.atoms.push_back(trb.build());
      }
    } else {
      size_t ac = a.arg_count();
      Atom::Builer b(a.sign(),a.pred(),ac);
      for(size_t i=0; i<ac; ++i) switch(a.arg(i).type()) {
        case Term::VAR: b.set_arg(i,f.arg(i)); break;
        case Term::FUN: {
          Term u(Var::make(cla.var_count++));
          b.set_arg(i,u);
          Atom::Builder ab(true,Atom::MONO_RED,3);
          ab.set_arg(0,a.arg(i));
          ab.set_arg(1,signature(a.arg(i));
          ab.set_arg(2,u);
          cla.derived.atoms.push_back(b.build());
          break;
        }
      }
      cla.derived.atoms.push_back(b.build());
    }
  }
}

DerAndClause conv(DerAndClause cla) {
  ClauseConverter cc;
  cc.cla = cla;
  cc.cla.derived.atoms.clear();
  for(auto a : cla.atoms) cc.flatten_once(a);
  return cc.cla;
}

OrForm conv(OrForm f) { for(auto &c : f.and_clauses) c = conv(c); return f; }

OrForm conv_and_append_axioms(OrForm f) {
  ArityCtx ctx; ctx.traverse(f);
  f = conv(f);

  for(auto fa : ctx.fun_arity) {
    if(fa.second) {
      AndClause mono1;
      Term target(Var::make(mono1.var_count++));
      Fun::Builder f1b(fa.first,fa.second);
      Fun::Builder f1sb(fa.first,fa.second);
      Fun::Builder f2b(fa.first,fa.second);
      for(size_t i=0; i<fa.second; ++i) {
        Term x1(Var::make(mono1.var_count++));
        Term x1s(Var::make(mono1.var_count++));
        Term x2(Var::make(mono1.var_count++));
        f1b.set_arg(i,x1);
        f1sb.set_arg(i,x1s);
        f2b.set_arg(i.x2);
        Atom::Builder rb(true,Atom::MONO_RED,3);
        rb.set_arg(0,x1);
        rb.set_arg(1,x1s);
        rb.set_arg(2,x2);
        mono1.atoms.push_back(rb.build());
      }
      Atom::Builder frb(false,Atom::MONO_RED,3);
      frb.set_arg(0,f1b.build());
      frb.set_arg(1,f1sb.build());
      frb.set_arg(2,target);
      mono1.atoms.push_back(frb.build());
      Atom::Builder etb(true,Atom::TRANS_TARGET,3);
      etb.set_arg(0,f2);
      etb.set_arg(1,fw);
      etb.set_arg(2,target);
      mono1.atoms.push_back(etb.build());
      f.and_clauses.push_back(DerAndClause(mono1));
    }
    DerAndClause mono2;
  }

}
  ArityCtx ctx;
  size_t fun_wrap(size_t i){ return ctx.fun_arity.size()+i; }
};

}

#endif  // LAZY_H_
