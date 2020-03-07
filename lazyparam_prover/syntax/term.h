#ifndef SYNTAX_TERM_H_
#define SYNTAX_TERM_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/memory/alloc.h"
#include "lazyparam_prover/memory/layout.h"
#include "lazyparam_prover/util/short.h"

namespace tableau {

struct Term0 {
  enum Type { VAR, FUN };
private:
  friend struct Var0;
  friend struct Fun0;
  friend struct Atom0;
  friend struct Constraint;
  using TYPE = Field<Type>;
  using VAR_END = Field<u64,TYPE>;
  using LAST_FIELD = VAR_END;
  u8 *ptr;
  Term0(u8 *_ptr) : ptr(_ptr) {}
public:
  u64 var_end() const { return VAR_END::ref(ptr); }
  Type type() const { return TYPE::ref(ptr); }
};

struct Var0 {
private:
  using ID = Field<u64,Term0::LAST_FIELD>;
  Term0 term;
  static Term0 _Var0(u64 id) {
    COUNTER("Var0(id)");
    auto ptr = ID::alloc();
    Term0::TYPE::ref(ptr) = Term0::VAR;
    Term0::VAR_END::ref(ptr) = id+1;
    ID::ref(ptr) = id;
    return Term0(ptr);
  }
public:
  explicit Var0(Term0 t) : term(t) {
    DEBUG if(Term0::TYPE::ref(t.ptr)!=Term0::VAR) error("Var(<type=%>)",Term0::TYPE::ref(term.ptr));
  }
  explicit Var0(u64 id) : Var0(_Var0(id)) {}
  explicit operator Term0() const { return term; }
  u64 id() const { return ID::ref(term.ptr); }
};

struct Fun0 {
private:
  using FUN = Field<u64,Term0::LAST_FIELD>;
  using ARGS = ArrayField<u8*,FUN>;
  Term0 term;
  static Term0 _Fun0(u64 fun, const vec<Term0> &args) {
    Builder b(fun,args.size());
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    return Term0(b.build());
  }
public:
  enum { EXTRA_CONST = u64(-1), VAR_WRAP = u64(-2), FUN_WRAP = u64(-3) };
  explicit Fun0(Term0 t) : term(t) {
    DEBUG if(Term0::TYPE::ref(term.ptr)!=Term0::FUN) error("Fun(<type=%>)",Term0::TYPE::ref(term.ptr));
  }
  explicit operator Term0() const { return term; }
  u64 fun() const { return FUN::ref(term.ptr); }
  u64 arg_count() const { return ARGS::size(term.ptr); }
  Term0 arg(size_t i) const { return Term0(ARGS::ref(term.ptr,i)); }
  explicit Fun0(u64 fun, const vec<Term0> &args) : Fun0(_Fun0(fun,args)) {}

  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(u64 _fun, u64 _arg_count) : ptr(ARGS::alloc(_arg_count)) {
      COUNTER("Fun::Builder");
      Term0::TYPE::ref(ptr) = Term0::FUN;
      Term0::VAR_END::ref(ptr) = 0;
      FUN::ref(ptr) = _fun;
    }
    void set_arg(size_t i, Term0 a) {
      ARGS::ref(ptr,i) = a.ptr;
      util::maxi(Term0::VAR_END::ref(ptr),a.var_end());
    }
    Fun0 build() { return Fun0(Term0(ptr)); } 
  };
};

struct Term {
  Term0::Type type() const { return term.type(); }
  u64 var_begin() const { return offset; }
  u64 var_end() const { return offset+term.var_end(); }
  explicit operator Term0();
private:
  Term(u64 _offset, Term0 _term) : offset(_offset), term(_term) {}
  u64 offset;
  Term0 term;
  friend struct Var;
  friend struct Fun;
  friend struct Atom;
};

struct Var {
  Var(Var0 v) : offset(0), var(v) {}
  explicit Var(Term t) : offset(t.offset), var(t.term) {}
  u64 id() const { return var.id()+offset; }
  explicit operator Term() const { return Term(offset,Term0(var)); }
  explicit operator Var0() const { return Var0(id()); }
private:
  u64 offset;
  Var0 var;
};

struct Fun {
  Fun(Fun0 f) : offset(0), fun_(f) {}
  explicit Fun(Term t) : offset(t.offset), fun_(t.term) {}  

  u64 fun() const { return fun_.fun(); }
  u64 arg_count() const { return fun_.arg_count(); }
  Term arg(size_t i) const { return Term(offset,fun_.arg(i)); }
  explicit operator Term() const { return Term(offset,Term0(fun_)); }
  explicit operator Fun0() const {
    Fun0::Builder b(fun(),arg_count());
    for(size_t i=arg_count(); i--;) b.set_arg(i,Term0(arg(i)));
    return b.build();
  }
private:
  u64 offset;
  Fun0 fun_;
};

Term::operator Term0() {
  switch(type()) {
    case Term0::VAR: return Term0(Var0(Var(*this)));
    case Term0::FUN: return Term0(Fun0(Fun(*this)));
    default: error("type = %",type());
  }
}

//TODO: replace with hash consing
inline bool operator==(Term x, Term y) {
  if(x.type()!=y.type()) return 0;
  switch(x.type()) {
  case Term0::FUN: {
    Fun fx(x),fy(y);
    if(fx.fun()!=fy.fun()) return 0;
    DEBUG if(fx.arg_count()!=fy.arg_count())
      error("fx.arg_count() = %, fy.arg_count() = %",fx.arg_count(),fy.arg_count());
    for(size_t i=fx.arg_count(); i--;)
      if(!(fx.arg(i)==fy.arg(i))) return 0;
    return 1;
  }
  case Term0::VAR:
    return Var(x).id()==Var(y).id();
  default:
    error("Term<type=%> == Term<type=%>",x.type(),y.type());
  }
}

inline bool operator!=(Term x, Term y) { return !(x==y); }

}  // namespace tableau

#endif  // SYNTAX_TERM_H_
