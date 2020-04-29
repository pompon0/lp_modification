#ifndef SYNTAX_TERM_H_
#define SYNTAX_TERM_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/memory/alloc.h"
#include "lazyparam_prover/memory/layout.h"
#include "lazyparam_prover/util/short.h"

namespace tableau {

struct VarRange {
  size_t begin,end;
  bool empty() const { return begin>=end; }
  VarRange & operator|=(const VarRange &r){
    if(r.empty()){ return *this; }
    if(empty()){ *this = r; return *this; }
    util::mini(begin,r.begin);
    util::maxi(end,r.end);
    return *this;
  }
  VarRange operator+(size_t offset) const {
    return {begin+offset,end+offset};
  }
  bool operator==(const VarRange &b) const {
    return begin==b.begin && end==b.end;
  }
};

struct Term {
  friend struct Var;
  friend struct Fun;
  enum Type { VAR, FUN };
private:
  using TYPE = Field<Type>;
  using LAST_FIELD = TYPE;
  u8 *ptr;
  size_t offset;
  Term(u8 *_ptr, size_t _offset) : ptr(_ptr), offset(_offset) {}
public:
  Type type() const { return TYPE::ref(ptr); }
  VarRange var_range() const;
  Term shift(size_t _offset) const { return Term(ptr,offset+_offset); }
};

struct Var {
private:
  Term term;
public:
  explicit Var(Term t) : term(t) {
    DEBUG if(Term::TYPE::ref(t.ptr)!=Term::VAR) error("Var(<type=%>)",Term::TYPE::ref(t.ptr));
  }
  explicit Var(u64 id) : term(Term::LAST_FIELD::alloc(),id) { Term::TYPE::ref(term.ptr) = Term::VAR; }
  explicit operator Term() const { return term; }
  u64 id() const { return term.offset; }
  VarRange var_range() const { return {term.offset,term.offset+1}; }
  Var shift(size_t offset) const { return Var(term.shift(offset)); }
};

struct Fun {
private:
  using VAR_RANGE = Field<VarRange,Term::LAST_FIELD>;
  using FUN = Field<u64,VAR_RANGE>;
  using ARGS = ArrayField<Term,FUN>;
  Term term;
  static Term _Fun(u64 fun, const vec<Term> &args) { FRAME("_Fun(%)",fun);
    Builder b(fun,args.size());
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    return Term(b.build());
  }
public:
  enum { EXTRA_CONST = u64(-1), VAR_WRAP = u64(-2), FUN_WRAP = u64(-3) };
  explicit Fun(Term t) : term(t) {
    DEBUG if(Term::TYPE::ref(t.ptr)!=Term::FUN) error("Fun(<type=%>)",Term::TYPE::ref(t.ptr));
  }
  explicit Fun(u64 fun, const vec<Term> &args) : Fun(_Fun(fun,args)) {}
  explicit operator Term() const { return term; }
  u64 fun() const { return FUN::ref(term.ptr); }
  u64 arg_count() const { return ARGS::size(term.ptr); }
  Term arg(size_t i) const { return ARGS::ref(term.ptr,i).shift(term.offset); }
  VarRange var_range() const { return VAR_RANGE::ref(term.ptr)+term.offset; }
  Fun shift(size_t offset) const { return Fun(term.shift(offset)); }

  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(u64 _fun, u64 _arg_count) : ptr(ARGS::alloc(_arg_count)) {
      COUNTER("Fun::Builder");
      VAR_RANGE::ref(ptr) = {0,0};
      Term::TYPE::ref(ptr) = Term::FUN;
      FUN::ref(ptr) = _fun;
      //TODO: in DEBUG mode, check if all elements have been set
    }
    void set_arg(size_t i, Term a) { FRAME("set_arg(%)",i);
      ARGS::ref(ptr,i) = a;
      VAR_RANGE::ref(ptr) |= a.var_range();
    }
    Fun build(){ return Fun(Term(ptr,0)); } 
  };
};

VarRange Term::var_range() const { FRAME("var_range()");
  switch(type()) {
  case VAR: return Var(*this).var_range();
  case FUN: return Fun(*this).var_range();
  default: error("<type=%>.var_range()",type());
  }
}


//TODO: replace with hash consing
inline bool operator==(Term x, Term y) {
  if(x.type()!=y.type()) return 0;
  switch(x.type()) {
  case Term::FUN: {
    Fun fx(x),fy(y);
    if(fx.fun()!=fy.fun()) return 0;
    DEBUG if(fx.arg_count()!=fy.arg_count())
      error("fx.arg_count() = %, fy.arg_count() = %",fx.arg_count(),fy.arg_count());
    for(size_t i=fx.arg_count(); i--;)
      if(!(fx.arg(i)==fy.arg(i))) return 0;
    return 1;
  }
  case Term::VAR:
    return Var(x).id()==Var(y).id();
  default:
    error("Term<type=%> == Term<type=%>",x.type(),y.type());
  }
}

inline bool operator!=(Term x, Term y) { return !(x==y); }

}  // namespace tableau

#endif  // SYNTAX_TERM_H_
