#ifndef SYNTAX_ATOM_H_
#define SYNTAX_ATOM_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct Atom0 {
private:
  friend struct Atom;
  friend struct OrClause0;
  using VAR_END = Field<u64>;
  using PRED = Field<u64,VAR_END>;
  using ARGS = ArrayField<u8*,PRED>;
  u8 *ptr;
  bool sign_;
  u64 id_; // used to identify atom (for indexing)
  Atom0(u8 *_ptr, bool _sign, u64 _id) : ptr(_ptr), sign_(_sign), id_(_id) {}
public:
  enum Pred {
    EQ = u64(-1),
    EQ_TRANS_POS = u64(-2),
    EQ_TRANS_NEG = u64(-3),
    EQ_SYMM = u64(-4),
    MONO_RED = u64(-5),
    TRANS_RED = u64(-6),
    TRANS_TARGET = u64(-7),
    PRED_MIN = TRANS_TARGET,
  };

  u64 var_end() const { return VAR_END::ref(ptr); }
  inline bool sign() const { return sign_; }
  inline u64 pred() const { return PRED::ref(ptr); }
  inline u64 arg_count() const { return ARGS::size(ptr); }
  inline Term0 arg(size_t i) const { return Term0(ARGS::ref(ptr,i)); }
  inline u64 id() const { return id_; } 

  explicit Atom0(bool sign, u64 pred, const vec<Term0> &args) {
    Builder b(sign,pred,args.size());
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    *this = b.build();
  }

  static inline Atom0 eq(bool sign, Term0 l, Term0 r) {
    Builder b(sign,EQ,2);
    b.set_arg(0,l);
    b.set_arg(1,r);
    return b.build();
  }


  struct Builder {
  private:
    bool sign_;
    u8 *ptr;
  public:
    Builder(bool _sign, u64 _pred, u64 _arg_count) : sign_(_sign), ptr(ARGS::alloc(_arg_count)) {
      COUNTER("Atom::Builder");
      VAR_END::ref(ptr) = 0;
      PRED::ref(ptr) = _pred;
      DEBUG for(size_t i=0; i<_arg_count; ++i) ARGS::ref(ptr,i) = 0;
    }
    inline void set_arg(size_t i, Term0 a){
      ARGS::ref(ptr,i) = a.ptr;
      util::maxi(VAR_END::ref(ptr),a.var_end());
    }
    inline Atom0 build() {
      DEBUG for(size_t i=0; i<ARGS::size(ptr); ++i) if(!ARGS::ref(ptr,i)) error("Atom::build() arg(%) not set",i);
      return Atom0(ptr,sign_,0);
    }
  };

  Atom0 neg() const { Atom0 a{*this}; a.sign_ = !a.sign_; return a; }
};

struct Atom {
  u64 var_begin() const { return offset; }
  u64 var_end() const { return offset+atom.var_end(); }

  inline bool sign() const { return atom.sign(); }
  inline u64 pred() const { return atom.pred(); }
  inline u64 arg_count() const { return atom.arg_count(); }
  inline Term arg(size_t i) const { return Term(offset,atom.arg(i)); }
  inline u64 id() const { return atom.id(); } 
  inline Atom neg() const { return Atom(offset,atom.neg()); }
  Atom(Atom0 _atom) : Atom(0,_atom) {}
  explicit operator Atom0() const {
    Atom0::Builder b(sign(),pred(),arg_count());
    for(size_t i=arg_count(); i--;) b.set_arg(i,Term0(arg(i)));
    return b.build();
  }
private:
  Atom(u64 _offset, Atom0 _atom) : offset(_offset), atom(_atom) {}
  u64 offset;
  Atom0 atom;
  friend struct OrClause;
};

inline bool operator==(Atom x, Atom y) {
  if(x.pred()!=y.pred()) return 0;
  DEBUG if(x.arg_count()!=y.arg_count())
    error("x.arg_count() = %, y.arg_count() = %",x.arg_count(),y.arg_count());
  if(x.sign()!=y.sign()) return 0;
  for(size_t i=x.arg_count(); i--;) if(x.arg(i)!=y.arg(i)) return 0;
  return 1;
}

inline bool operator!=(Atom x, Atom y) { return !(x==y); }

}  // tableau

#endif  // SYNTAX_ATOM_H_
