#ifndef LAZY_H_
#define LAZY_H_

struct AbstractFunCtx {
  AbstractFunCtx(OrForm f){ ctx.traverse(NotAndForm(f)); }
 
  Term conv(Term t) const {
    switch(t.type()) {
      case Term::VAR: {
        Fun::Builder b(VAR_WRAP,1);
        b.set_arg(0,t);
        return b.build();
      }
      case Term::FUN: {
        Fun f(t);
        size_t ac = f.arg_count();
        Fun::Builder b(fun_wrap(ac),ac+1);
        b.set_arg(0,Fun::Builder(f.fun(),0).build());
        for(size_t i=0; i<ac; ++i) b.set_arg(i+1,conv(f.arg(i)));
        return b.build();
      }
    }
  }
  Atom conv(Atom a) const {
    Atom::Builder b(a.sign(),a.pred(),a.arg_count());
    for(size_t i=a.arg_count(); i--;) b.set_arg(i,conv(a.arg(i)));
    return b.build();
  }
  AndClause conv(AndClause c) const { for(auto &a : c.atoms) a = conv(a); return c; }
  DerAndClause conv(DerAndClause c) const { c.derived = conv(c.derived); return c; }
  OrForm conv(OrForm f) const { for(auto &c : f.and_clauses) c = ctx.conv(c); return f; }

  OrForm initial_flattening(OrForm f) {
    // f(x) = g(y) -> N(fun(f,tup_1(v(x))),a), a = b, N(fun(tup_1(v(y)),b)
  }

  OrForm append_mono_axioms(OrForm) {
    //TODO:
    // N(a,b) -N(tup_i(a),tup_i(b)) - mono cost
    // D(x,x') -N(x,x') - 0 cost
    // N(fun(f,t),x) => fun(f,t) != v(x) - 0 cost
    // N(fun(f,t),fun(f,t') => N(t,t')
    // D(x,x') -N(x,y) nFV(x',y) - 0 cost
    // D(a,b) -D(t(f,a),t(f,b)) - 0 cost
    // -D(v(x),x) - 0 cost
  }

  OrForm append_trans_axioms(OrForm) {

  }


private:
  ArityCtx ctx;
  size_t fun_wrap(size_t i){ return ctx.fun_arity.size()+i; }
};

OrForm abstract_fun(OrForm f) {
  AbstractFunCtx ctx(f);

}

OrForm lazy_transform(OrForm f) {
  
  return f;
}

#endif  // LAZY_H_
