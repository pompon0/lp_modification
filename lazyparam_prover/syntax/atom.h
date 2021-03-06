#ifndef SYNTAX_ATOM_H_
#define SYNTAX_ATOM_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct Atom {
private:
  using VAR_RANGE = memory::Field<VarRange>;
  using PRED = memory::Field<u64,VAR_RANGE>;
  using ARGS = memory::ArrayField<Term,PRED>;
  using PTR = ARGS;
  PTR ptr;
  size_t offset;
  bool sign_;
  u64 id_; // used to identify atom (for indexing)
  // strong only indicates that only strong connections may apply to this atom.
  // It is useful for emulating resolution on the input of the problem, while
  // keeping the number of clauses fixed. This in turn allows for avoiding some
  // unifications (without losing completeness) and improves cost balancing if
  // exactly 1 unification is possible.
  bool strong_only_; 
  INL Atom(PTR _ptr, size_t _offset, bool _sign, u64 _id, bool _strong_only)
    : ptr(_ptr), offset(_offset), sign_(_sign), id_(_id), strong_only_(_strong_only) {}
public:
  enum Pred {
    EQ = u64(-1),
    EQ_TRANS_POS = u64(-2),
    EQ_TRANS_NEG = u64(-3),
    EQ_SYMM = u64(-4),
    PRED_MIN = EQ_SYMM,
  };

  INL VarRange var_range() const { return ptr.VAR_RANGE::ref()+offset; }
  INL Atom shift(size_t _offset) const { return Atom(ptr,offset+_offset,sign_,id_,strong_only_); }
  INL Atom neg() const { return Atom(ptr,offset,!sign_,id_,strong_only_); }
  INL Atom set_id(size_t _id) const { return Atom(ptr,offset,sign_,_id,strong_only_); }
  INL Atom set_strong_only() const { return Atom(ptr,offset,sign_,id_,true); }
  template<typename Alloc> INL Atom replace_arg(Alloc &a, size_t i, Term t) const {
    Builder b(a,sign(),pred(),arg_count(),strong_only());
    for(size_t i=arg_count(); i--;) b.set_arg(i,arg(i));
    b.set_arg(i,t);
    return b.build();
  }
  INL bool sign() const { return sign_; }
  INL u64 pred() const { return ptr.PRED::ref(); }
  INL u64 arg_count() const { return ptr.ARGS::size(); }
  INL Term arg(size_t i) const { return ptr.ARGS::ref(i).shift(offset); }
  INL u64 id() const { return id_; } 
  INL bool strong_only() const { return strong_only_; }

  template<typename Alloc> Atom(Alloc &a, bool sign, u64 pred, const vec<Term> &args) {
    Builder b(a,sign,pred,args.size(),false);
    for(size_t i=0; i<args.size(); ++i) b.set_arg(i,args[i]);
    *this = b.build();
  }

  template<typename Alloc> static Atom eq(Alloc &a, bool sign, Term l, Term r) {
    return Builder(a,sign,EQ,2,false).set_arg(0,l).set_arg(1,r).build();
  }

  struct Builder {
  private:
    bool sign_;
    PTR ptr;
    bool strong_only_;
  public:
    template<typename Alloc> Builder(Alloc &a, bool _sign, u64 _pred, u64 _arg_count, bool _strong_only)
        : sign_(_sign), ptr(PTR::alloc(a,_arg_count)), strong_only_(_strong_only) {
      COUNTER("Atom::Builder");
      ptr.VAR_RANGE::ref() = {0,0};
      ptr.PRED::ref() = _pred;
      //DEBUG for(size_t i=0; i<_arg_count; ++i) ARGS::ref(ptr,i) = 0;
    }
    INL Builder& set_arg(size_t i, Term a){
      ptr.ARGS::ref(i) = a;
      ptr.VAR_RANGE::ref() |= a.var_range();
      return *this;
    }
    INL Atom build() {
      //DEBUG for(size_t i=0; i<ARGS::size(ptr); ++i) if(!ARGS::ref(ptr,i)) error("Atom::build() arg(%) not set",i);
      return Atom(ptr,0,sign_,0,strong_only_);
    }
  };
};

INL inline bool operator==(Atom x, Atom y) {
  if(x.pred()!=y.pred()) return 0;
  DEBUG if(x.arg_count()!=y.arg_count())
    error("x.arg_count() = %, y.arg_count() = %",x.arg_count(),y.arg_count());
  if(x.sign()!=y.sign()) return 0;
  for(size_t i=x.arg_count(); i--;) if(x.arg(i)!=y.arg(i)) return 0;
  return 1;
}

INL inline bool operator!=(Atom x, Atom y) { return !(x==y); }

}  // tableau

#endif  // SYNTAX_ATOM_H_
