#ifndef SYNTAX_ATOM_H_
#define SYNTAX_ATOM_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct Atom {
private:
  using VAR_RANGE = Field<VarRange>;
  using PRED = Field<u64,VAR_RANGE>;
  using ARGS = ArrayField<Term,PRED>;
  u8 *ptr;
  size_t offset;
  bool sign_;
  u64 id_; // used to identify atom (for indexing)
  Atom(u8 *_ptr, size_t _offset, bool _sign, u64 _id) : ptr(_ptr), offset(_offset), sign_(_sign), id_(_id) {}
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

  VarRange var_range() const { return VAR_RANGE::ref(ptr)+offset; }
  Atom shift(size_t _offset) const { return Atom(ptr,offset+_offset,sign_,id_); }
  Atom set_id(size_t _id) const { return Atom(ptr,offset,sign_,_id); }
  Atom neg() const { return Atom(ptr,offset,!sign_,id_); }
  
  inline bool sign() const { return sign_; }
  inline u64 pred() const { return PRED::ref(ptr); }
  inline u64 arg_count() const { return ARGS::size(ptr); }
  inline Term arg(size_t i) const { return ARGS::ref(ptr,i).shift(offset); }
  inline u64 id() const { return id_; } 

  explicit Atom(bool sign, u64 pred, const vec<Term> &args) {
    Builder b(sign,pred,args.size());
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    *this = b.build();
  }

  static inline Atom eq(bool sign, Term l, Term r) {
    return Builder(sign,EQ,2).set_arg(0,l).set_arg(1,r).build();
  }

  struct Builder {
  private:
    bool sign_;
    u8 *ptr;
  public:
    Builder(bool _sign, u64 _pred, u64 _arg_count) : sign_(_sign), ptr(ARGS::alloc(_arg_count)) {
      COUNTER("Atom::Builder");
      VAR_RANGE::ref(ptr) = {0,0};
      PRED::ref(ptr) = _pred;
      //DEBUG for(size_t i=0; i<_arg_count; ++i) ARGS::ref(ptr,i) = 0;
    }
    inline Builder& set_arg(size_t i, Term a){
      ARGS::ref(ptr,i) = a;
      VAR_RANGE::ref(ptr) |= a.var_range();
      return *this;
    }
    inline Atom build() {
      //DEBUG for(size_t i=0; i<ARGS::size(ptr); ++i) if(!ARGS::ref(ptr,i)) error("Atom::build() arg(%) not set",i);
      return Atom(ptr,0,sign_,0);
    }
  };
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
