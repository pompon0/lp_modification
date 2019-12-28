#ifndef LAZY_H_
#define LAZY_H_

// define signature function
//   s(f(a,b)) := f(s(a),s(b))
//   s(x) := v
// do initial flattening
//   f(a,b) = g(c)  ==>  (f(a,b),s(f(a,b))) -> x /\ (g(c),s(g(c))) -> x [f(a,b)>=x, g(c)>=x]
//   f(a,b) != g(c)  ==>  (a,s(a)) -> x /\ (b,s(b)) -> y /\ (f(x,y),F) != (g(z),F) /\ (c,s(c)) -> z [a>=x,b>=y,c>=z] [f(x,y)!=g(z) is applied at =]
// generate signature aware mono axioms
//   [3] (f(y,z),f(u1,u2)) !-> x /\ (f(y',z'),F) [=] x /\ (y,u1) -> y' /\ (z,u2) -> z' [y>=y', z>=z', f(y',z')>=x, f(y,z)>f(y',z')] (last one is the reason to apply the axiom at all).
//   [0] (y,f(u1,u2)) !-> x /\ (y,F) [=] x [y>=x]
//   [0] (x,v) !-> x [it applies to variable sig only, because you have other refl axiom as well]
// generate signature aware trans axioms
//   [3] (x,F) = (y,ys) /\ (z,zs) = (x,F) /\ (y,ys) != (z,zs) [x>y,x>z]
//     we can additionally always enforce x>y in (x,xs) = (y,ys), by applying appropriate symmetry axiom between every 2 transitivity axioms
//   [3] (x,F) ![=] z /\ (x,xs) = (y,ys) /\ (y,ys) [=] z [x > y, z!=x] [x>=z is applied at [=]]
//   [0] (x,xs) ![=] x [note that this axiom is stronger than some instances of other with ![=] literals]
// generate signature aware symmetry axiom
//   [1] (x,xs) = (y,ys) /\ (y,ys) != (x,xs) [y>x]

namespace tableau {
namespace lazy {

// a = b /\ b = c /\ a != c
AndClause trans_axiom(Term a, Term b, Term c) {
  AndClause cla;
  cla.atoms = {
    Atom::eq(true,a,b),
    Atom::eq(true,b,c),
    Atom::eq(false,a,c)
  };
  return cla;
}

// a = b /\ b != a
AndClause symm_axiom(Term a, Term b) {
  AndClause cla;
  cla.atoms = {
    Atom::eq(true,a,b),
    Atom::eq(false,b,a)
  };
  return cla;
}

struct ClauseConverter {
  DerAndClause cla;

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
        AndClause ma;
        for(size_t i=0; i<ac; ++i) switch(f.arg(i).type()) {
          case Term::VAR: b.set_arg(i,f.arg(i)); break;
          case Term::FUN: {
            Term u(Var::make(cla.derived.var_count++));
            b.set_arg(i,u);
            // (a,s(a)) -> x [a>=x]
            Atom::Builder ab(true,Atom::MONO_RED,3);
            ab.set_arg(0,f.arg(i));
            ab.set_arg(1,signature(f.arg(i)));
            ab.set_arg(2,u);
            cla.derived.atoms.push_back(ab.build());
            cla.constraints.push_back(Constraint::le(u,f.arg(i)));
            ma.atoms.push_back(Atom::eq(true,f.arg(i),u));
            break;
          }
        }
        Term ff(b.build());
        // a = x /\ b = y /\ f(a,b) != f(x,y)
        ma.atoms.push_back(Atom::eq(false,t,ff));
        cla.source.push_back(ma);
        return ff;
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
            Term x(Var::make(cla.derived.var_count++));
            // (g(c),F) -> x  [f(a,b)>=x, g(c)>=x]
            //   f(a,b) = x /\ x = g(c) /\ f(a,b) != g(c)
            //   g(c) = x /\ x != g(c)
            Atom::Builder rb(true,Atom::MONO_RED,3);
            rb.set_arg(0,r);
            rb.set_arg(1,signature(r));
            rb.set_arg(2,x);
            cla.derived.atoms.push_back(rb.build());
            cla.source.push_back(trans_axiom(l,x,r));
            cla.source.push_back(symm_axiom(r,x));
            cla.constraints.push_back(Constraint::le(x,r));
            r = x;
          }
        }
        // (f(a,b),F) -> x
        Atom::Builder lb(true,Atom::MONO_RED,3);
        lb.set_arg(0,l);
        lb.set_arg(1,signature(l));
        lb.set_arg(2,r);
        cla.derived.atoms.push_back(lb.build());
        cla.constraints.push_back(Constraint::le(r,l));
      } else {
        // (f(x,y),F) != (g(z),F)
        //   f(x,y) = f(a,b) /\ f(a,b) = g(c) /\ f(x,y) != g(c)
        //   f(x,y) = g(c) /\ g(c) = g(z) /\ f(x,y) != g(z)
        //   f(a,b) = f(x,y) /\ f(x,y) != f(a,b)
        Term fl = flatten_once(l);
        Term fr = flatten_once(r);
        Atom::Builder trb(false,Atom::TRANS_RED,4);
        trb.set_arg(0,fl);
        trb.set_arg(1,l.type()==Term::FUN ? fw : vw);
        trb.set_arg(2,fr);
        trb.set_arg(3,r.type()==Term::FUN ? fw : vw);
        cla.derived.atoms.push_back(trb.build());
        cla.source.push_back(trans_axiom(fl,l,r));
        cla.source.push_back(trans_axiom(fl,r,fr));
        cla.source.push_back(symm_axiom(l,fl));
      }
    } else {
      size_t ac = a.arg_count();
      // p(u)
      //   d = u /\ -p(d) /\ p(u)
      Atom::Builder b(a.sign(),a.pred(),ac);
      AndClause ma;
      for(size_t i=0; i<ac; ++i) switch(a.arg(i).type()) {
        case Term::VAR: b.set_arg(i,a.arg(i)); break;
        case Term::FUN: {
          Term u(Var::make(cla.derived.var_count++));
          b.set_arg(i,u);
          // (d,s(d)) -> u
          Atom::Builder ab(true,Atom::MONO_RED,3);
          ab.set_arg(0,a.arg(i));
          ab.set_arg(1,signature(a.arg(i)));
          ab.set_arg(2,u);
          cla.derived.atoms.push_back(ab.build());
          ma.atoms.push_back(Atom::eq(true,a.arg(i),u));
          break;
        }
      }
      ma.atoms.push_back(a.neg());
      ma.atoms.push_back(b.build());
      cla.derived.atoms.push_back(b.build());
      cla.source.push_back(ma);
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

OrForm conv_and_append_axioms(OrForm f) { FRAME("conv_and_append_axioms()");
  ArityCtx ctx; ctx.traverse(NotAndForm(f));
  f = conv(f);
  
  Term vw(Fun::Builder(Fun::VAR_WRAP,0).build());
  Term fw(Fun::Builder(Fun::FUN_WRAP,0).build());

  for(auto fa : ctx.fun_arity) {
    if(fa.second) {
      DerAndClause cla;
      AndClause mono1;
      AndClause mono1s;
      Term target(Var::make(mono1.var_count++));
      Fun::Builder f1b(fa.first,fa.second);
      Fun::Builder f1sb(fa.first,fa.second);
      Fun::Builder f2b(fa.first,fa.second);
      // (y,u1) -> y' /\ (z,u2) -> z' [y>=y', z>=z']
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
        mono1s.atoms.push_back(Atom::eq(true,x1,x2));
        cla.constraints.push_back(Constraint::le(x2,x1));
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
      mono1s.atoms.push_back(Atom::eq(false,f1,f2));
      // (f(y',z'),F) [=] x [f(y',z')>=x, f(y,z)>f(y',z')]
      Atom::Builder etb(true,Atom::TRANS_TARGET,3);
      etb.set_arg(0,f2);
      etb.set_arg(1,fw);
      etb.set_arg(2,target);
      mono1.atoms.push_back(etb.build()); 
      cla.constraints.push_back(Constraint::le(target,f2));
      cla.constraints.push_back(Constraint::lt(f2,f1));
      // (f(y,z),f(u1,u2)) !-> x /\ (f(y',z'),F) [=] x /\ (y,u1) -> y' /\ (z,u2) -> z'
      //   y = y' /\ z = z' /\ f(y,z) != f(y',z')
      //   f(y,z) = f(y',z') /\ f(y',z') = x /\ f(y,z) != x
      cla.cost = 3;
      cla.derived = mono1;
      cla.source.push_back(mono1s);
      cla.source.push_back(trans_axiom(f1,f2,target));
      f.and_clauses.push_back(cla);
    }

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
    // (y,F) [=] x [y>=x]
    Atom::Builder tb(true,Atom::TRANS_TARGET,3);
    tb.set_arg(0,y);
    tb.set_arg(1,fw);
    tb.set_arg(2,x);
    mono2.atoms.push_back(tb.build());
    DerAndClause cla;
    cla.cost = 0;
    cla.derived = mono2;
    cla.constraints.push_back(Constraint::le(x,y));
    f.and_clauses.push_back(cla);
  }

  {
    AndClause refl;
    AndClause refls;
    // (x,v) !-> x
    Term x(Var::make(refl.var_count++));
    Atom::Builder b(false,Atom::MONO_RED,3);
    b.set_arg(0,x);
    b.set_arg(1,vw);
    b.set_arg(2,x);
    refl.atoms.push_back(b.build());
    refls.atoms.push_back(Atom::eq(false,x,x));
    DerAndClause cla;
    cla.cost = 0;
    cla.derived = refl;
    cla.source.push_back(refls);
    f.and_clauses.push_back(cla);
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
    // (x,F) = (y,ys) /\ (z,zs) = (x,F) /\ (y,ys) != (z,zs) [x>y,x>z]
    DerAndClause cla;
    cla.cost = 1;
    cla.derived = trans1;
    cla.source.push_back(trans_axiom(z,x,y));
    cla.source.push_back(symm_axiom(z,y));
    cla.constraints.push_back(Constraint::lt(y,x));
    cla.constraints.push_back(Constraint::lt(z,x));
    f.and_clauses.push_back(cla);
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
    Atom::Builder tb(true,Atom::TRANS_TARGET,3);
    tb.set_arg(0,y);
    tb.set_arg(1,ys);
    tb.set_arg(2,z);
    trans2.atoms.push_back(tb.build());
    // (x,F) ![=] z /\ (x,xs) = (y,ys) /\ (y,ys) [=] z  [x > y, z!=x]
    DerAndClause cla;
    cla.cost = 1;
    cla.derived = trans2;
    cla.source.push_back(trans_axiom(x,y,z));
    cla.constraints.push_back(Constraint::lt(y,x));
    cla.constraints.push_back(Constraint::neq(z,x));
    f.and_clauses.push_back(cla);
  }

  {
    AndClause refl2;
    AndClause refl2s;
    Term x(Var::make(refl2.var_count++));
    Term xs(Var::make(refl2.var_count++));
    // (x,xs) ![=] x
    Atom::Builder tbn(false,Atom::TRANS_TARGET,3);
    tbn.set_arg(0,x);
    tbn.set_arg(1,xs);
    tbn.set_arg(2,x);
    refl2.atoms.push_back(tbn.build());
    refl2s.atoms.push_back(Atom::eq(false,x,x));
    // (x,xs) ![=] x
    DerAndClause cla;
    cla.cost = 0;
    cla.derived = refl2;
    cla.source.push_back(refl2s);
    f.and_clauses.push_back(cla);
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
    // (x,xs) = (y,ys) /\ (y,ys) != (x,xs) [y>x]
    DerAndClause cla;
    cla.cost = 1;
    cla.derived = symm;
    cla.source.push_back(symm_axiom(x,y));
    cla.constraints.push_back(Constraint::lt(x,y));
    f.and_clauses.push_back(cla);
  }

  return f;
}

} // namespace lazy
} // namespace tableau

#endif  // LAZY_H_
