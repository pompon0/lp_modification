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
    COUNTER("Var::make");
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
  
  static Fun slow_make(u64 fun, const vec<Term> &args) {
    Builder b(fun,args.size());
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    return b.build();
  }

  struct Builder {
  private:
    u64 *ptr;
  public:
    Builder(u64 _fun, u64 _arg_count) : ptr(alloc(ARGS+_arg_count)) {
      COUNTER("Fun::Builder");
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

struct Atom {
private:
  friend struct OrClause;
  enum { SIGN, PRED, ARG_COUNT, ARGS };
  u64 *ptr;
  u64 var_offset;
  explicit Atom(u64 *_ptr, u64 _var_offset) : ptr(_ptr), var_offset(_var_offset) {}
public:
  enum { EQ = u64(-1), PRED_MIN = EQ };

  inline bool sign(){ return ptr[SIGN]; }
  inline u64 pred() const { return ptr[PRED]; }
  inline u64 arg_count() const { return ptr[ARG_COUNT]; }
  inline Term arg(size_t i) const { return Term((u64*)ptr[ARGS+i],var_offset); }

  static inline Atom eq(bool sign, Term l, Term r) {
    Builder b(sign,EQ,2);
    b.set_arg(0,l);
    b.set_arg(1,r);
    return b.build();
  }

  struct Builder {
  private:
    u64 *ptr;
  public:
    Builder(bool _sign, u64 _pred, u64 _arg_count) : ptr(alloc(ARGS+_arg_count)) {
      COUNTER("Atom::Builder");
      ptr[SIGN] = _sign;
      ptr[PRED] = _pred;
      ptr[ARG_COUNT] = _arg_count;
      DEBUG for(size_t i=0; i<_arg_count; ++i) ptr[ARGS+i] = 0;
    }
    inline void set_arg(size_t i, Term a){
      DEBUG if(a.var_offset!=0) error("Atom::set_arg(): var_offset = %, want %",a.var_offset,0);
      ptr[ARGS+i] = (u64)a.ptr;
    }
    inline Atom build(){
      DEBUG for(size_t i=0; i<ptr[ARG_COUNT]; ++i) if(!ptr[ARGS+i]) error("Atom::build() arg(%) not set",i);
      return Atom(ptr,0);
    }
  };

  Atom neg() const {
    u64 *end = ptr+ARGS+arg_count();
    u64 *ptr2 = alloc(end-ptr);
    for(auto x = ptr, y = ptr2; x<end;) *y++ = *x++;
    ptr2[SIGN] = !ptr2[SIGN];
    return Atom(ptr2,var_offset);
  }
};

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
private:
  enum { ATOM_COUNT, VAR_COUNT, ATOMS };
  u64 *ptr;
  u64 var_offset;
  OrClause(u64 *_ptr, u64 _var_offset) : ptr(_ptr), var_offset(_var_offset) {}
public:
  size_t var_count() const { return var_offset+ptr[VAR_COUNT]; }
  size_t atom_count() const { return ptr[ATOM_COUNT]; } 
  Atom atom(size_t i) const {
    DEBUG if(i>=atom_count()) error("<atom_count=%>.arg(%)",atom_count(),i);
    return Atom((u64*)ptr[ATOMS+i],var_offset);
  }
  AndClause neg() const;

  OrClause shift(size_t offset) const { FRAME("OrClause::shift()");
    DEBUG if(var_offset!=0) error("var_offset = %, want %",var_offset,0);
    return OrClause(ptr,offset);
  }

  struct Builder {
  private:
    u64 *ptr;
  public:
    Builder(u64 _atom_count, u64 _var_count) : ptr(alloc(ATOMS+_atom_count)) {
      ptr[VAR_COUNT] = _var_count;
      ptr[ATOM_COUNT] = _atom_count;
    }
    void set_atom(size_t i, Atom a) { FRAME("OrClause.Builder.set_atom()");
      DEBUG if(a.var_offset) error("a.var_offset = %, want %",a.var_offset,0);
      ptr[ATOMS+i] = (u64)a.ptr;
    }
    OrClause build(){ return OrClause(ptr,0); }
  };

  bool operator==(const OrClause &cla) const {
    if(cla.atom_count()!=atom_count()) return 0;
    for(size_t i=ptr[ATOM_COUNT]; i--;) if(cla.atom(i)!=atom(i)) return 0;
    return 1;
  }
  bool operator!=(const OrClause &cla) const { return !(*this==cla); }
};

inline OrClause AndClause::neg() const {
  OrClause::Builder b(atoms.size(),var_count);
  for(size_t i=atoms.size(); i--;) b.set_atom(i,atoms[i].neg());
  return b.build();
}

inline AndClause OrClause::neg() const {
  AndClause d(var_count());
  for(size_t i=0; i<atom_count(); ++i) d.atoms.push_back(atom(i).neg());
  return d;
}

struct DerAndClause;
struct DerOrClause;

struct DerAndClause {
  DerAndClause(){}
  explicit DerAndClause(AndClause cla) : derived(cla), source{cla} {}
  AndClause derived;
  vec<AndClause> source;
  DerOrClause neg() const;
};

struct DerOrClause {
  explicit DerOrClause(OrClause cla) : DerOrClause(cla,List<OrClause>(cla)) {}
  DerOrClause(OrClause _derived, List<OrClause> _source) : DerOrClause(0,_derived,_source) {}
  DerAndClause neg() const;
  DerOrClause shift(size_t _offset) const {
    DEBUG if(offset_) error("offset = %, want %",offset_,0);
    return DerOrClause(_offset,derived_,source_);
  }
  OrClause derived() const { return derived_.shift(offset_); }
  vec<OrClause> source() const {
    vec<OrClause> s; for(auto l=source_; !l.empty(); l = l.tail()) s.push_back(l.head().shift(offset_));
    return s;
  }
private:
  DerOrClause(size_t _offset, OrClause _derived, List<OrClause> _source) : offset_(_offset), derived_(_derived), source_(_source) {}
  size_t offset_;
  OrClause derived_;
  List<OrClause> source_;
};

DerOrClause DerAndClause::neg() const {
  List<OrClause> source_neg;
  for(const auto &c : source) source_neg += c.neg();
  return DerOrClause(derived.neg(),source_neg);
}

DerAndClause DerOrClause::neg() const {
  DerAndClause cla;
  cla.derived = derived().neg();
  for(auto c : source()) {
    cla.source.push_back(c.neg());
  }
  return cla;
}

struct OrForm {
  vec<DerAndClause> and_clauses;
  OrForm(){}
  explicit OrForm(const NotAndForm &);
};

struct NotAndForm {
  vec<DerOrClause> or_clauses;
  NotAndForm(){}
  explicit NotAndForm(const OrForm &);
};

inline NotAndForm::NotAndForm(const OrForm &f) {
  for(const auto &c : f.and_clauses) or_clauses.push_back(c.neg());
}

inline OrForm::OrForm(const NotAndForm &f) {
  for(const auto &c : f.or_clauses) and_clauses.push_back(c.neg());
}

using Proof = DerAndClause;

static_assert(sizeof(u64*)==sizeof(u64));
static_assert(sizeof(Term)==2*sizeof(u64));
static_assert(sizeof(Var)==sizeof(Term));
static_assert(sizeof(Fun)==sizeof(Term));
static_assert(sizeof(Atom)==sizeof(Term));
static_assert(sizeof(OrClause)==sizeof(Term));

#endif // PRED_H_
