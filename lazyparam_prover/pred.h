#ifndef PRED_H_
#define PRED_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/alloc.h"

struct Term {
private:
  friend struct Var;
  friend struct Fun;
  friend struct Atom;
  enum { TYPE, SIZE };
  u64 *ptr;
  u64 var_offset;
  Term(u64 *_ptr, u64 _var_offset) : ptr(_ptr), var_offset(_var_offset) {}
public:
  enum Type { VAR, FUN };
  Type type(){ return Type(ptr[TYPE]); }
};

struct Var {
private:
  enum { ID = Term::SIZE, SIZE };
  Term term;
public:
  explicit Var(Term t) : term(t) {
    DEBUG if(term.ptr[Term::TYPE]!=Term::VAR) error("Var(<type=%>)",term.ptr[Term::TYPE]);
  }
  explicit operator Term() { return term; }
  u64 id(){ return term.ptr[ID]+term.var_offset; }
  
  static Var make(u64 id) {
    auto ptr = alloc(SIZE);
    ptr[Term::TYPE] = Term::VAR;
    ptr[ID] = id;
    return Var(Term(ptr,0));
  }
};

struct Fun {
private:
  enum { FUN = Term::SIZE, ARG_COUNT, ARGS };
  Term term;
public:
  enum { EXTRA_CONST = -1 };
  Fun(Term t) : term(t) {
    DEBUG if(term.ptr[Term::TYPE]!=FUN) error("Fun(<type=%>)",term.ptr[Term::TYPE]);
  }
  explicit operator Term(){ return term; }
  u64 fun(){ return term.ptr[FUN]; }
  u64 arg_count(){ return term.ptr[ARG_COUNT]; }
  Term arg(size_t i) {
    DEBUG if(i>=arg_count()) error("<arg_count=%>.arg(%)",arg_count(),i);
    return Term((u64*)term.ptr[ARGS+i],term.var_offset);
  }
  
  struct Builder {
  private:
    u64 *ptr;
  public:
    Builder(u64 _fun, u64 _arg_count) : ptr(alloc(ARGS+_arg_count)) {
      ptr[Term::TYPE] = FUN;
      ptr[FUN] = _fun;
      ptr[ARG_COUNT] = _arg_count;
    }
    void set_arg(size_t i, Term a){
      DEBUG if(a.var_offset) error("Fun::Builder.set_arg(): var_offset = %, want 0",a.var_offset);
      ptr[ARGS+i] = (u64)a.ptr;
    }
    Fun build(){ return Fun(Term(ptr,0)); }
  };
};

inline bool has_var(Term t, u64 v) {
  switch(t.type()) {
  case Term::VAR: return Var(t).id()==v;
  case Term::FUN: {
    Fun f(t);
    for(auto i=f.arg_count(); i--;)
      if(has_var(f.arg(i),v)) return 1;
    return 0;
  }
  }
  error("has_var(<type=%>,v",t.type());
}

struct Atom {
private:
  enum { SIGN, PRED, VAR_OFFSET, ARG_COUNT, ARGS };
  u64 *ptr;
  explicit Atom(u64 *_ptr) : ptr(_ptr) {}
public:
  enum { EQ = u64(-1) };

  inline bool sign(){ return ptr[SIGN]; }
  inline u64 pred(){ return ptr[PRED]; }
  inline u64 arg_count(){ return ptr[ARG_COUNT]; }
  inline Term arg(size_t i){ return Term((u64*)ptr[ARGS+i],ptr[VAR_OFFSET]); }
  inline u64 var_offset(){ return ptr[VAR_OFFSET]; }

  static inline Atom eq(bool sign, Term l, Term r) {
    Builder b(sign,EQ,2,0);
    b.set_arg(0,l);
    b.set_arg(1,r);
    return b.build();
  }

  struct Builder {
  private:
    u64 *ptr;
  public:
    Builder(bool _sign, u64 _pred, u64 _arg_count, u64 _var_offset) : ptr(alloc(ARGS+_arg_count)) {
      ptr[SIGN] = _sign;
      ptr[PRED] = _pred;
      ptr[ARG_COUNT] = _arg_count;
      ptr[VAR_OFFSET] = _var_offset;
      DEBUG for(size_t i=0; i<_arg_count; ++i) ptr[ARGS+i] = 0;
    }
    inline void set_arg(size_t i, Term a){
      DEBUG if(a.var_offset!=ptr[VAR_OFFSET]) error("Atom::set_arg(): var_offset = %, want %",a.var_offset,ptr[VAR_OFFSET]);
      ptr[ARGS+i] = (u64)a.ptr;
    }
    inline Atom build(){
      DEBUG for(size_t i=0; i<ptr[ARG_COUNT]; ++i) if(!ptr[ARGS+i]) error("Atom::build() arg(%) not set",i);
      return Atom(ptr);
    }
  };

  Atom neg() {
    u64 *end = ptr+ARGS+arg_count();
    u64 *ptr2 = alloc(end-ptr);
    for(auto x = ptr, y = ptr2; x<end;) *y++ = *x++;
    ptr2[SIGN] = !ptr2[SIGN];
    return Atom(ptr2);
  }

  Atom with_offset(size_t offset) {
    u64 *end = ptr+ARGS+arg_count();
    u64 *ptr2 = alloc(end-ptr);
    for(auto x = ptr, y = ptr2; x<end;) *y++ = *x++;
    ptr2[VAR_OFFSET] = offset;
    return Atom(ptr2);
  }
};

static_assert(sizeof(u64*)==sizeof(u64));
static_assert(sizeof(Term)==2*sizeof(u64));
static_assert(sizeof(Var)==sizeof(Term));
static_assert(sizeof(Fun)==sizeof(Term));
static_assert(sizeof(Atom)==sizeof(u64));

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

inline bool operator==(Atom x, Atom y) {
  if(x.pred()!=y.pred()) return 0;
  DEBUG if(x.arg_count()!=y.arg_count())
    error("x.arg_count() = %, y.arg_count() = %",x.arg_count(),y.arg_count());
  if(x.sign()!=y.sign()) return 0;
  for(size_t i=x.arg_count(); i--;)
    if(!(x.arg(i)==y.arg(i))) return 0;
  return 1;
}

inline bool operator!=(Atom x, Atom y) { return !(x==y); }

struct OrClause;
struct AndClause;
struct NotAndForm;
struct OrForm;

struct AndClause {
  AndClause(size_t _var_count = 0) : var_count(_var_count) {}
  size_t var_count;
  vec<Atom> atoms;
  OrClause neg() const;
};

struct OrClause {
  OrClause(size_t _var_count = 0) : var_count(_var_count) {}
  size_t var_count;
  vec<Atom> atoms;
  AndClause neg() const;

  bool operator==(const OrClause &cla) const { return atoms==cla.atoms; }
  bool operator!=(const OrClause &cla) const { return !(*this==cla); }
};

inline OrClause AndClause::neg() const {
  OrClause d(var_count);
  for(auto a : atoms) d.atoms.push_back(a.neg());
  return d;
}

inline AndClause OrClause::neg() const {
  AndClause d(var_count);
  for(auto a : atoms) d.atoms.push_back(a.neg());
  return d;
}

struct OrForm {
  vec<AndClause> and_clauses;
  OrForm(){}
  explicit OrForm(const NotAndForm &);
};

struct NotAndForm {
  vec<OrClause> or_clauses;
  NotAndForm(){}
  explicit NotAndForm(const OrForm &);
};

inline NotAndForm::NotAndForm(const OrForm &f) {
  for(const auto &c : f.and_clauses) or_clauses.push_back(c.neg());
}

inline OrForm::OrForm(const NotAndForm &f) {
  for(const auto &c : f.or_clauses) and_clauses.push_back(c.neg());
}

using Proof = OrForm;

#endif // PRED_H_
