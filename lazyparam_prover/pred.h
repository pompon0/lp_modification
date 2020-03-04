#ifndef PRED_H_
#define PRED_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/alloc.h"

namespace tableau {

template<typename T> struct NoOffset;

struct Term {
private:
  friend struct Var;
  friend struct Fun;
  friend struct Atom;
  friend struct Constraint;
  enum { TYPE, SIZE };
  u64 *ptr;
  u64 var_offset;
  Term(u64 *_ptr, u64 _var_offset) : ptr(_ptr), var_offset(_var_offset) {}
public:
  enum Type { VAR, FUN };
  Type type(){ return Type(ptr[TYPE]); }
  inline NoOffset<Term> drop_offset();
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
  
  NoOffset<Var> drop_offset();
  static NoOffset<Var> make(u64 id);
};

struct Fun {
private:
  enum { FUN = Term::SIZE, ARG_COUNT, ARGS };
  Term term;
public:
  enum { EXTRA_CONST = u64(-1), VAR_WRAP = u64(-2), FUN_WRAP = u64(-3) };
  Fun(Term t) : term(t) {
    DEBUG if(term.ptr[Term::TYPE]!=FUN) error("Fun(<type=%>)",term.ptr[Term::TYPE]);
  }
  explicit operator Term(){ return term; }
  u64 fun() const { return term.ptr[FUN]; }
  u64 arg_count() const { return term.ptr[ARG_COUNT]; }
  Term arg(size_t i) const {
    DEBUG if(i>=arg_count()) error("<arg_count=%>.arg(%)",arg_count(),i);
    return Term((u64*)term.ptr[ARGS+i],term.var_offset);
  }

  NoOffset<Fun> drop_offset();
  static NoOffset<Fun> slow_make(u64 fun, const vec<NoOffset<Term>> &args);

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
    void set_arg(size_t i, NoOffset<Term> a);
    NoOffset<Fun> build();
  };
};

template<> struct NoOffset<Term> : Term {
  explicit NoOffset(Term t) : Term(t) {} 
  explicit NoOffset(NoOffset<Var> v);
  explicit NoOffset(NoOffset<Fun> f);
};

template<> struct NoOffset<Var> : Var {
  explicit NoOffset(Var v) : Var(v) {}
  explicit NoOffset(NoOffset<Term> t) : Var(t) {}
};

template<> struct NoOffset<Fun> : Fun {
  explicit NoOffset(Fun f) : Fun(f) {}
  explicit NoOffset(NoOffset<Term> t) : Fun(t) {}
  NoOffset<Term> arg(size_t i) const;
};

NoOffset<Term>::NoOffset(NoOffset<Var> v) : Term(v) {}
NoOffset<Term>::NoOffset(NoOffset<Fun> f) : Term(f) {}

NoOffset<Term> NoOffset<Fun>::arg(size_t i) const { return NoOffset<Term>(Fun::arg(i)); }

NoOffset<Var> Var::drop_offset() { return make(id()); }

NoOffset<Var> Var::make(u64 id) {
  COUNTER("Var::make");
  auto ptr = alloc(SIZE);
  ptr[Term::TYPE] = Term::VAR;
  ptr[ID] = id;
  return NoOffset<Var>(Var(Term(ptr,0)));
}

NoOffset<Fun> Fun::drop_offset() {
  Builder b(fun(),arg_count());
  for(size_t i=arg_count(); i--;) b.set_arg(i,arg(i).drop_offset());
  return b.build();
}

NoOffset<Fun> Fun::slow_make(u64 fun, const vec<NoOffset<Term>> &args) {
  Builder b(fun,args.size());
  for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
  return b.build();
}

void Fun::Builder::set_arg(size_t i, NoOffset<Term> a){ ptr[ARGS+i] = (u64)a.ptr; }
NoOffset<Fun> Fun::Builder::build(){ return NoOffset<Fun>(Fun(Term(ptr,0))); }

inline NoOffset<Term> Term::drop_offset() {
  switch(type()) {
    case VAR: return NoOffset<Term>(Var(*this).drop_offset());
    case FUN: return NoOffset<Term>(Fun(*this).drop_offset());
  default: error("type = %",type());
  }
}

struct Atom {
private:
  friend struct OrClause;
  enum { SIGN, PRED, ARG_COUNT, ARGS };
  u64 *ptr;
  u64 var_offset;
  bool sign_;
  u64 id_; // used to identify atom (for indexing)
  explicit Atom(u64 *_ptr, u64 _var_offset, u64 _id) : ptr(_ptr), var_offset(_var_offset), id_(_id) {}
public:
  enum {
    EQ = u64(-1),
    EQ_TRANS_POS = u64(-2),
    EQ_TRANS_NEG = u64(-3),
    EQ_SYMM = u64(-4),
    MONO_RED = u64(-5),
    TRANS_RED = u64(-6),
    TRANS_TARGET = u64(-7),
    PRED_MIN = TRANS_TARGET,
  };

  inline bool sign() const { return sign_; }
  inline u64 pred() const { return ptr[PRED]; }
  inline u64 arg_count() const { return ptr[ARG_COUNT]; }
  inline Term arg(size_t i) const { return Term((u64*)ptr[ARGS+i],var_offset); }
  inline u64 id() const { return id_; } 

  NoOffset<Atom> drop_offset();
  static NoOffset<Atom> slow_make(bool sign, u64 pred, const vec<NoOffset<Term>> &args);
  static NoOffset<Atom> eq(bool sign, NoOffset<Term> l, NoOffset<Term> r);

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
    inline void set_arg(size_t i, NoOffset<Term> a){ ptr[ARGS+i] = (u64)a.ptr; }
    inline NoOffset<Atom> build();
  };

  Atom neg() const { Atom a{*this}; a.sign_ = !a.sign_; return a; }
};

template<> struct NoOffset<Atom> : Atom {
  NoOffset(Atom a) : Atom(a) {}
  NoOffset neg() const { return NoOffset(Atom::neg()); }
};

NoOffset<Atom> Atom::drop_offset() {
  Builder b(sign(),pred(),arg_count());
  for(size_t i=arg_count(); i--;) b.set_arg(i,arg(i).drop_offset());
  return b.build();
}

NoOffset<Atom> Atom::eq(bool sign, NoOffset<Term> l, NoOffset<Term> r) {
  Builder b(sign,EQ,2);
  b.set_arg(0,l);
  b.set_arg(1,r);
  return b.build();
}

NoOffset<Atom> Atom::slow_make(bool sign, u64 pred, const vec<NoOffset<Term>> &args) {
  Builder b(sign,pred,args.size());
  for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
  return b.build();
}

NoOffset<Atom> Atom::Builder::build(){
  DEBUG for(size_t i=0; i<ptr[ARG_COUNT]; ++i) if(!ptr[ARGS+i]) error("Atom::build() arg(%) not set",i);
  return NoOffset<Atom>(Atom(ptr,0,0));
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

struct OrClause {
private:
  friend NoOffset<OrClause>;
  enum { ATOM_COUNT, VAR_COUNT, ATOMS };
  u64 *ptr;
  u64 var_offset_;
  u64 id_offset_;
  OrClause(u64 *_ptr, u64 _var_offset, u64 _id_offset) : ptr(_ptr), var_offset_(_var_offset), id_offset_(_id_offset) {}
public:
  size_t var_count() const { return var_offset_+ptr[VAR_COUNT]; }
  size_t atom_count() const { return ptr[ATOM_COUNT]; } 
  Atom atom(size_t i) const {
    DEBUG if(i>=atom_count()) error("<atom_count=%>.arg(%)",atom_count(),i);
    return Atom((u64*)ptr[ATOMS+i],var_offset_,id_offset_+i);
  }
  AndClause neg() const;

  OrClause set_id_offset(size_t _id_offset) const { FRAME("OrClause::set_id_offset()");
    DEBUG if(id_offset_!=0) error("id_offset = %, want %",id_offset_,0);
    return OrClause(ptr,var_offset_,_id_offset);
  }

  struct Builder {
  private:
    u64 *ptr;
  public:
    Builder(u64 _atom_count, u64 _var_count) : ptr(alloc(ATOMS+_atom_count)) {
      ptr[VAR_COUNT] = _var_count;
      ptr[ATOM_COUNT] = _atom_count;
    }
    void set_atom(size_t i, NoOffset<Atom> a) { FRAME("OrClause.Builder.set_atom()");
      DEBUG if(a.var_offset) error("a.var_offset = %, want %",a.var_offset,0);
      ptr[ATOMS+i] = (u64)a.ptr;
    }
    NoOffset<OrClause> build();
  };

  bool operator==(const OrClause &cla) const {
    if(cla.atom_count()!=atom_count()) return 0;
    for(size_t i=ptr[ATOM_COUNT]; i--;) if(cla.atom(i)!=atom(i)) return 0;
    return 1;
  }
  bool operator!=(const OrClause &cla) const { return !(*this==cla); }
};

struct AndClause {
  struct Iso {
    using From = OrClause;
    using To = AndClause;
    From from(To c){ return c.neg(); }
    To to(From c){ return c.neg(); }
  };
  size_t var_count() const { return neg_or_clause.var_count(); }
  size_t atom_count() const { return neg_or_clause.atom_count(); }
  Atom atom(size_t i) const { return neg_or_clause.atom(i).neg(); }
  OrClause neg() const { return neg_or_clause; }
private:
  explicit AndClause(OrClause _neg_or_clause) : neg_or_clause(_neg_or_clause) {}
  OrClause neg_or_clause;
  friend AndClause OrClause::neg() const;
};

inline AndClause OrClause::neg() const { return AndClause(*this); }

template<> struct NoOffset<OrClause> : OrClause {
  NoOffset(OrClause c) : OrClause(c) {}
  NoOffset<Atom> atom(size_t i) const { return NoOffset<Atom>(OrClause::atom(i)); }
  NoOffset<AndClause> neg() const;
  OrClause shift(size_t _var_offset) const { FRAME("OrClause::shift()");
    return OrClause(ptr,_var_offset,id_offset_);
  }
};

template<> struct NoOffset<AndClause> : AndClause {
  NoOffset(AndClause c) : AndClause(c) {}
  NoOffset<Atom> atom(size_t i) const { return NoOffset<Atom>(AndClause::atom(i)); }
  NoOffset<OrClause> neg() const { return NoOffset<OrClause>(AndClause::neg()); }
};

NoOffset<AndClause> NoOffset<OrClause>::neg() const { return NoOffset<AndClause>(OrClause::neg()); }

NoOffset<OrClause> OrClause::Builder::build(){ return NoOffset<OrClause>(OrClause(ptr,0,0)); }



static_assert(sizeof(u64*)==sizeof(u64));
static_assert(sizeof(Term)==2*sizeof(u64));
static_assert(sizeof(Var)==sizeof(Term));
static_assert(sizeof(Fun)==sizeof(Term));
static_assert(sizeof(Atom)<=sizeof(Term)+2*sizeof(u64));
static_assert(sizeof(OrClause)==sizeof(AndClause));

}  // namespace tableau

#endif // PRED_H_
