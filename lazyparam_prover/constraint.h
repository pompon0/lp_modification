#ifndef CONSTRAINT_H_
#define CONSTRAINT_H_

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/show.h"

namespace tableau {

struct OrderAtom {
  enum Relation { L = 1, G = 2, E = 4, LE = L|E, GE = G|E, NE = L|G, U = L|G|E };
  enum Status { TRUE, FALSE, UNKNOWN };
  struct TermPair {
    Term a,b;
    friend bool operator==(const TermPair a, const TermPair b) {
      return a.a==b.a && a.b==b.b;
    }
    TermPair shift(size_t offset) const {
      auto t = *this;
      t.a = t.a.shift(offset);
      t.b = t.b.shift(offset);
      return t;
    }
  };
  
  Status status() const { return status_; }
  VarRange var_range() const { return VAR_RANGE::ref(ptr); }
  OrderAtom shift(size_t _offset) const { return OrderAtom(ptr,offset+_offset,status_,done); }

  Relation rel() const { return RELATION::ref(ptr); }
  size_t pair_count() const { return TERM_PAIRS::size(ptr); }
  TermPair pair(size_t i) const { return TERM_PAIRS::ref(ptr,i).shift(offset); }

  // OrderAtom -> (Term -> Term -> OrderAtom::Relation) -> OrderAtom
  template<typename CMP> OrderAtom reduce(CMP &cmp) const {
    if(status()!=UNKNOWN) return *this;
    for(size_t _done = done, size = pair_count();; _done++) {
      auto p = pair(_done);
      auto r = cmp.cmp(p.a,p.b);
      bool last = _done==size-1;
      if(!last && r==E) continue;
      auto got = decide(r,last);
      auto want = RELATION::ref(ptr);
      if((got&want)==got) return OrderAtom(ptr,offset,TRUE,_done);
      if((got&want)==0) return OrderAtom(ptr,offset,FALSE,_done);
      return OrderAtom(ptr,offset,UNKNOWN,_done);
    }
  }

  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(Relation rel, size_t pair_count) : ptr(TERM_PAIRS::alloc(pair_count)) {
      VAR_RANGE::ref(ptr) = {0,0};
      RELATION::ref(ptr) = rel;
    }
    Builder& set_pair(size_t i, TermPair p) {
      TERM_PAIRS::ref(ptr,i) = p;
      VAR_RANGE::ref(ptr) |= p.a.var_range() |= p.b.var_range();
      return *this;
    }
    OrderAtom build() {
      return OrderAtom(ptr,0,TERM_PAIRS::size(ptr) ? UNKNOWN : RELATION::ref(ptr)&E ? TRUE : FALSE,0);
    }
  };
  
  // ignores sign
  static OrderAtom neq(Atom l, Atom r) {
    if(l.pred()!=r.pred()) return OrderAtom(0,0,TRUE,0);
    DEBUG if(l.arg_count()!=r.arg_count()) error("l.arg_count() = %, r.arg_count() = %",show(l),show(r));
    Builder b(NE,l.arg_count());
    for(size_t i=l.arg_count(); i--;) b.set_pair(i,{l.arg(i),r.arg(i)});
    return b.build();
  }

  OrderAtom(Relation rel, Term l, Term r) : OrderAtom(Builder(rel,1).set_pair(0,{l,r}).build()) {}

  friend bool operator==(const OrderAtom &a, const OrderAtom &b) {
    bool ok = a.rel()==b.rel();
    ok &= a.pair_count()==b.pair_count();
    if(!ok) return false;
    for(size_t i=a.pair_count(); i--;) ok &= a.pair(i)==b.pair(i);
    return ok;
  }

private:
  using VAR_RANGE = Field<VarRange>;
  using RELATION = Field<Relation,VAR_RANGE>;
  using TERM_PAIRS = ArrayField<TermPair,RELATION>;

  u8 *ptr;
  size_t offset;
  Status status_;
  size_t done;
  OrderAtom(u8 *_ptr, size_t _offset, Status _status, size_t _done) : ptr(_ptr), offset(_offset), status_(_status), done(_done) {}

  Relation decide(Relation rel, bool last) const {
    switch(rel) {
      case L: return L;
      case G: return G;
      case E: DEBUG if(!last) error("decide(E,last=true)"); return E;
      case LE: return last ? LE : U;
      case GE: return last ? GE : U;
      case NE: return NE;
      case U: return U;
      default: error("decide(%)",rel);
    }
  }
};

}  // namespace tableau

#endif  // CONSTRAINT_H_