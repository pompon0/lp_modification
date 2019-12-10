#ifndef LAZY_H_
#define LAZY_H_

// define signature function
//   s(f(a,b)) := f(s(a),s(b)
//   s(x) := v
// do initial flattening
//   f(a,b) = g(c)  ==>  (f(a,b),s(f(a,b))) -> x /\ (g(c),s(g(c))) -> x [f(a,b)>=x, g(c)>=x]
//   f(a,b) != g(c)  ==>  (a,s(a)) -> x /\ (b,s(b)) -> y /\ (f(x,y),F) != (g(z),F) /\ (c,s(c)) -> z [a>=x,b>=y,c>=z]
// generate signature aware mono axioms
//   [3] (f(y,z),f(u1,u2)) !-> x /\ (f(y',z'),F) [=] x /\ (y,u1) -> y' /\ (z,u2) -> z' [f(y,z)>f(y',z')]
//   [0] (f(y,z),f(u1,u2)) !-> x /\ (f(y,z),F) [=] x [f(y,z)>x]
//   [0] (x,v) !-> x [you can assume that sides are different if this axiom doesn't match]
// generate signature aware trans axioms
//   [3] (x,F) = (y,ys) /\ (z,zs) = (x,F) /\ (y,ys) != (z,zs) [x>y,x>z]
//     we can additionally always enforce x>y in (x,xs) = (y,ys), by applying appropriate symmetry axiom between every 2 transitivity axioms
//   [3] (x,F) ![=] z /\ (x,xs) = (y,ys) /\ (y,ys) [=] z [x>y]
//   [0] (x,xs) ![=] x
// generate signature aware symmetry axiom
//   [1] (x,xs) = (y,ys) /\ (y,ys) != (x,xs) [y>x]

namespace lazy {

struct ClauseConverter {
  DerAndClause cla; // TODO: source

  Term signature(Term t) const { FRAME("signature(%)",show(t));
    switch(t.type()) {
      case Term::VAR: {
        return Term(Fun::Builder(Fun::VAR_WRAP,0).build());
      }
      case Term::FUN: {
        Fun f(t);
        size_t ac = f.arg_count();
        Fun::Builder b(f.fun(),ac);
        for(size_t i=0; i<ac; ++i) b.set_arg(i,signature(f.arg(i)));
        return Term(b.build());
      }
    }
    error("t.type() = %",t.type());
  }
  
  Term flatten_once(Term t) { FRAME("flatten_once(%)",show(t));
    switch(t.type()) {
      case Term::VAR: return t;
      case Term::FUN: {
        Fun f(t);
        size_t ac = f.arg_count();
        Fun::Builder b(f.fun(),ac);
        for(size_t i=0; i<ac; ++i) switch(f.arg(i).type()) {
          case Term::VAR: b.set_arg(i,f.arg(i)); break;
          case Term::FUN: {
            Term u(Var::make(cla.derived.var_count++));
            b.set_arg(i,u);
            // (a,s(a)) -> x
            Atom::Builder ab(true,Atom::MONO_RED,3);
            ab.set_arg(0,f.arg(i));
            ab.set_arg(1,signature(f.arg(i)));
            ab.set_arg(2,u);
            cla.derived.atoms.push_back(ab.build());
            break;
          }
        }
        return Term(b.build());
      }
    }
    error("t.type() = %",t.type());
  }

  void flatten_once(Atom a) { FRAME("flatten_once(%)",show(a));
    Term vw(Fun::Builder(Fun::VAR_WRAP,0).build());
    Term fw(Fun::Builder(Fun::FUN_WRAP,0).build());
    if(a.pred()==Atom::EQ) {
      auto l = a.arg(0);
      auto r = a.arg(1);
      if(a.sign()) {
        if(r.type()==Term::FUN) switch(l.type()) {
          case Term::VAR: std::swap(l,r); break;
          case Term::FUN: {           
            Term u(Var::make(cla.derived.var_count++));
            // (g(c),F) -> x
            Atom::Builder rb(true,Atom::MONO_RED,3);
            rb.set_arg(0,r);
            rb.set_arg(1,signature(r));
            rb.set_arg(2,u);
            cla.derived.atoms.push_back(rb.build());
            r = u;
          }
        }
        Atom::Builder lb(true,Atom::MONO_RED,3);
        lb.set_arg(0,l);
        lb.set_arg(1,signature(l));
        lb.set_arg(2,r);
        cla.derived.atoms.push_back(lb.build());
      } else {
        Atom::Builder trb(false,Atom::TRANS_RED,4);
        trb.set_arg(0,flatten_once(l));
        trb.set_arg(1,l.type()==Term::FUN ? fw : vw);
        trb.set_arg(2,flatten_once(r));
        trb.set_arg(3,r.type()==Term::FUN ? fw : vw);
        cla.derived.atoms.push_back(trb.build());
      }
    } else {
      size_t ac = a.arg_count();
      Atom::Builder b(a.sign(),a.pred(),ac);
      for(size_t i=0; i<ac; ++i) switch(a.arg(i).type()) {
        case Term::VAR: b.set_arg(i,a.arg(i)); break;
        case Term::FUN: {
          Term u(Var::make(cla.derived.var_count++));
          b.set_arg(i,u);
          Atom::Builder ab(true,Atom::MONO_RED,3);
          ab.set_arg(0,a.arg(i));
          ab.set_arg(1,signature(a.arg(i)));
          ab.set_arg(2,u);
          cla.derived.atoms.push_back(b.build());
          break;
        }
      }
      cla.derived.atoms.push_back(b.build());
    }
  }
};

DerAndClause conv(DerAndClause cla) { FRAME("conv(%)",show(cla));
  ClauseConverter cc;
  cc.cla = cla;
  cc.cla.derived.atoms.clear();
  for(auto a : cla.derived.atoms) cc.flatten_once(a);
  return cc.cla;
}

OrForm conv(OrForm f) { for(auto &c : f.and_clauses) c = conv(c); return f; }

OrForm conv_and_append_axioms(OrForm f) {
  ArityCtx ctx; ctx.traverse(NotAndForm(f));
  f = conv(f);
  
  Term vw(Fun::Builder(Fun::VAR_WRAP,0).build());
  Term fw(Fun::Builder(Fun::FUN_WRAP,0).build());

  for(auto fa : ctx.fun_arity) {
    if(fa.first != Atom::EQ && fa.second) {
      AndClause mono1;
      Term target(Var::make(mono1.var_count++));
      Fun::Builder f1b(fa.first,fa.second);
      Fun::Builder f1sb(fa.first,fa.second);
      Fun::Builder f2b(fa.first,fa.second);
      // (y,u1) -> y' /\ (z,u2) -> z'
      for(size_t i=0; i<fa.second; ++i) {
        Term x1(Var::make(mono1.var_count++));
        Term x1s(Var::make(mono1.var_count++));
        Term x2(Var::make(mono1.var_count++));
        f1b.set_arg(i,x1);
        f1sb.set_arg(i,x1s);
        f2b.set_arg(i,x2);
        Atom::Builder rb(true,Atom::MONO_RED,3);
        rb.set_arg(0,x1);
        rb.set_arg(1,x1s);
        rb.set_arg(2,x2);
        mono1.atoms.push_back(rb.build());
      }
      Term f1(f1b.build());
      Term f1s(f1sb.build());
      Term f2(f2b.build());
      // (f(y,z),f(u1,u2)) !-> x 
      Atom::Builder frb(false,Atom::MONO_RED,3);
      frb.set_arg(0,f1);
      frb.set_arg(1,f1s);
      frb.set_arg(2,target);
      mono1.atoms.push_back(frb.build());
      // (f(y',z'),F) [=] x
      Atom::Builder etb(true,Atom::TRANS_TARGET,3);
      etb.set_arg(0,f2);
      etb.set_arg(1,fw);
      etb.set_arg(2,target);
      mono1.atoms.push_back(etb.build());
      f.and_clauses.push_back(DerAndClause(3,mono1));
    }

    if(fa.first != Atom::EQ) {
      AndClause mono2;
      // (y,f(u1,u2)) !-> x 
      Term x(Var::make(mono2.var_count++));
      Term y(Var::make(mono2.var_count++));
      Fun::Builder fsb(fa.first,fa.second);
      for (size_t i=0; i<fa.second; ++i) {
        fsb.set_arg(i,Term(Var::make(mono2.var_count++)));
      }
      Atom::Builder rb(false,Atom::MONO_RED,3);
      rb.set_arg(0,y);
      rb.set_arg(1,Term(fsb.build()));
      rb.set_arg(2,x);
      mono2.atoms.push_back(rb.build());
      // (y,F) [=] x
      Atom::Builder tb(true,Atom::TRANS_TARGET,3);
      tb.set_arg(0,y);
      tb.set_arg(1,fw);
      tb.set_arg(2,x);
      mono2.atoms.push_back(tb.build());
      f.and_clauses.push_back(DerAndClause(0,mono2));
    }
  }

  {
    AndClause refl;
    // (x,v) !-> x
    Term x(Var::make(refl.var_count++));
    Atom::Builder b(false,Atom::MONO_RED,3);
    b.set_arg(0,x);
    b.set_arg(1,vw);
    b.set_arg(2,x);
    refl.atoms.push_back(b.build());
    f.and_clauses.push_back(DerAndClause(0,refl));
  }

  {
    AndClause trans1;
    Term x(Var::make(trans1.var_count++));
    Term y(Var::make(trans1.var_count++));
    Term ys(Var::make(trans1.var_count++));
    Term z(Var::make(trans1.var_count++));
    Term zs(Var::make(trans1.var_count++));
    // (x,F) = (y,ys)
    Atom::Builder rb1(true,Atom::TRANS_RED,4);
    rb1.set_arg(0,x);
    rb1.set_arg(1,fw);
    rb1.set_arg(2,y);
    rb1.set_arg(3,ys);
    trans1.atoms.push_back(rb1.build());
    // (z,zs) = (x,F)
    Atom::Builder rb2(true,Atom::TRANS_RED,4);
    rb2.set_arg(0,z);
    rb2.set_arg(1,zs);
    rb2.set_arg(2,x);
    rb2.set_arg(3,fw);
    trans1.atoms.push_back(rb2.build());
    // (y,ys) != (z,zs)
    Atom::Builder rbn(false,Atom::TRANS_RED,4);
    rbn.set_arg(0,y);
    rbn.set_arg(1,ys);
    rbn.set_arg(2,z);
    rbn.set_arg(3,zs);
    trans1.atoms.push_back(rbn.build());
    f.and_clauses.push_back(DerAndClause(1,trans1));
  }

  {
    AndClause trans2;
    Term x(Var::make(trans2.var_count++));
    Term xs(Var::make(trans2.var_count++));
    Term y(Var::make(trans2.var_count++));
    Term ys(Var::make(trans2.var_count++));
    Term z(Var::make(trans2.var_count++));
    // (x,F) ![=] z
    Atom::Builder tbn(false,Atom::TRANS_TARGET,3);
    tbn.set_arg(0,x);
    tbn.set_arg(1,fw);
    tbn.set_arg(2,z);
    trans2.atoms.push_back(tbn.build());
    // (x,xs) = (y,ys)
    Atom::Builder rb(true,Atom::TRANS_RED,4);
    rb.set_arg(0,x);
    rb.set_arg(1,xs);
    rb.set_arg(2,y);
    rb.set_arg(3,ys);
    trans2.atoms.push_back(rb.build());
    // (y,ys) [=] z
    Atom::Builder tb(false,Atom::TRANS_TARGET,3);
    tb.set_arg(0,y);
    tb.set_arg(1,ys);
    tb.set_arg(2,z);
    trans2.atoms.push_back(tb.build());
    f.and_clauses.push_back(DerAndClause(1,trans2));
  }

  {
    AndClause refl2;
    Term x(Var::make(refl2.var_count++));
    Term xs(Var::make(refl2.var_count++));
    // (x,xs) ![=] x
    Atom::Builder tbn(false,Atom::TRANS_TARGET,3);
    tbn.set_arg(0,x);
    tbn.set_arg(1,xs);
    tbn.set_arg(2,x);
    refl2.atoms.push_back(tbn.build());
    f.and_clauses.push_back(DerAndClause(0,refl2));
  }

  {
    AndClause symm;
    Term x(Var::make(symm.var_count++));
    Term xs(Var::make(symm.var_count++));
    Term y(Var::make(symm.var_count++));
    Term ys(Var::make(symm.var_count++));
    // (x,xs) = (y,ys)
    Atom::Builder rb(true,Atom::TRANS_RED,4);
    rb.set_arg(0,x);
    rb.set_arg(1,xs);
    rb.set_arg(2,y);
    rb.set_arg(3,ys);
    symm.atoms.push_back(rb.build());
    // (y,ys) != (x,xs)
    Atom::Builder rbn(false,Atom::TRANS_RED,4);
    rbn.set_arg(0,y);
    rbn.set_arg(1,ys);
    rbn.set_arg(2,x);
    rbn.set_arg(3,xs);
    symm.atoms.push_back(rbn.build());
    f.and_clauses.push_back(DerAndClause(1,symm));
  }

  return f;
}

}

#endif  // LAZY_H_
