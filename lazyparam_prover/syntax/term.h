#ifndef SYNTAX_TERM_H_
#define SYNTAX_TERM_H_

#include "utils/short.h"
#include "utils/types.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct VarRange {
  size_t begin,end;
  INL bool empty() const { return begin>=end; }
  INL VarRange & operator|=(const VarRange &r){
    if(r.empty()){ return *this; }
    if(empty()){ *this = r; return *this; }
    util::mini(begin,r.begin);
    util::maxi(end,r.end);
    return *this;
  }
  INL VarRange operator+(size_t offset) const {
    return {begin+offset,end+offset};
  }
  INL bool operator==(const VarRange &b) const {
    return begin==b.begin && end==b.end;
  }
};

struct Term {
  friend struct Var;
  friend struct Fun;
  enum Type { VAR, FUN };
private:
  using TYPE = memory::Field<Type>;
  using LAST_FIELD = TYPE;
  u8 *ptr;
  size_t offset;
  INL Term(u8 *_ptr, size_t _offset) : ptr(_ptr), offset(_offset) {}
public:
  INL Type type() const { return TYPE::ref(ptr); }
  INL VarRange var_range() const;
  INL Term shift(size_t _offset) const { return Term(ptr,offset+_offset); }
};

struct Var {
private:
  Term term;
public:
  INL explicit Var(Term t) : term(t) {
    DEBUG if(Term::TYPE::ref(t.ptr)!=Term::VAR) error("Var(<type=%>)",Term::TYPE::ref(t.ptr));
  }
  template<typename Alloc> INL Var(Alloc &a, u64 id) : term(Term::LAST_FIELD::alloc(a),id) { Term::TYPE::ref(term.ptr) = Term::VAR; }
  INL explicit operator Term() const { return term; }
  INL u64 id() const { return term.offset; }
  INL VarRange var_range() const { return {term.offset,term.offset+1}; }
  INL Var shift(size_t offset) const { return Var(term.shift(offset)); }
};

struct Fun {
private:
  using VAR_RANGE = memory::Field<VarRange,Term::LAST_FIELD>;
  using FUN = memory::Field<u64,VAR_RANGE>;
  using ARGS = memory::ArrayField<Term,FUN>;
  Term term;
  template<typename Alloc> INL static Term _Fun(Alloc &a, u64 fun, const vec<Term> &args) { FRAME("_Fun(%)",fun);
    Builder b(a,fun,args.size());
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    return Term(b.build());
  }
public:
  enum { EXTRA_CONST = u64(-1), VAR_WRAP = u64(-2), FUN_WRAP = u64(-3) };
  INL explicit Fun(Term t) : term(t) {
    DEBUG if(Term::TYPE::ref(t.ptr)!=Term::FUN) error("Fun(<type=%>)",Term::TYPE::ref(t.ptr));
  }
  template<typename Alloc> INL Fun(Alloc &a, u64 fun, const vec<Term> &args) : Fun(_Fun(a,fun,args)) {}
  INL explicit operator Term() const { return term; }
  INL u64 fun() const { return FUN::ref(term.ptr); }
  INL u64 arg_count() const { return ARGS::size(term.ptr); }
  INL Term arg(size_t i) const { return ARGS::ref(term.ptr,i).shift(term.offset); }
  INL VarRange var_range() const { return VAR_RANGE::ref(term.ptr)+term.offset; }
  INL Fun shift(size_t offset) const { return Fun(term.shift(offset)); }
  template<typename Alloc> INL Fun replace_arg(Alloc &a, size_t i, Term t) const {
    Builder b(a,fun(),arg_count());
    for(size_t i=arg_count(); i--;) b.set_arg(i,arg(i));
    b.set_arg(i,t);
    return b.build();
  }
  struct Builder {
  private:
    u8 *ptr;
  public:
    template<typename Alloc> INL Builder(Alloc &a, u64 _fun, u64 _arg_count) : ptr(ARGS::alloc(a,_arg_count)) {
      COUNTER("Fun::Builder");
      VAR_RANGE::ref(ptr) = {0,0};
      Term::TYPE::ref(ptr) = Term::FUN;
      FUN::ref(ptr) = _fun;
      //TODO: in DEBUG mode, check if all elements have been set
    }
    INL void set_arg(size_t i, Term a) { FRAME("set_arg(%)",i);
      ARGS::ref(ptr,i) = a;
      VAR_RANGE::ref(ptr) |= a.var_range();
    }
    INL Fun build(){ return Fun(Term(ptr,0)); } 
  };
};

inline VarRange Term::var_range() const { FRAME("var_range()");
  switch(type()) {
  case VAR: return Var(*this).var_range();
  case FUN: return Fun(*this).var_range();
  }
  error("<type=%>.var_range()",type());
}


//TODO: replace with hash consing
static bool operator==(Term x, Term y) {
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

INL inline bool operator!=(Term x, Term y) { return !(x==y); }

}  // namespace tableau

#endif  // SYNTAX_TERM_H_
