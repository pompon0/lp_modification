#ifndef SYNTAX_CLAUSE_H_
#define SYNTAX_CLAUSE_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct AndClause0;

struct OrClause0 {
private:
  using VAR_END = Field<size_t>;
  using ATOMS = ArrayField<Atom0,VAR_END>;
  u8 *ptr;
  OrClause0(u8 *_ptr) : ptr(_ptr) {}
public:
  size_t var_end() const { return VAR_END::ref(ptr); }
  size_t atom_count() const { return ATOMS::size(ptr); } 
  Atom0 atom(size_t i) const {
    DEBUG if(i>=atom_count()) error("<atom_count=%>.arg(%)",atom_count(),i);
    return ATOMS::ref(ptr,i);
  }
  AndClause0 neg() const;

  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(size_t _atom_count) : ptr(ATOMS::alloc(_atom_count)) {
      VAR_END::ref(ptr) = 0;
    }
    void set_atom(size_t i, Atom0 a) { FRAME("OrClause0.Builder.set_atom()");
      DEBUG if(i>=ATOMS::size(ptr)) error("<atom_count=%>.set_atom(%)",ATOMS::size(ptr),i);
      ATOMS::ref(ptr,i) = a;
      util::maxi(VAR_END::ref(ptr),a.var_end());
    }
    OrClause0 build(){ return OrClause0(ptr); }
  }; 
};

struct AndClause0 {
  struct Iso {
    using From = OrClause0;
    using To = AndClause0;
    From from(To c){ return c.neg(); }
    To to(From c){ return c.neg(); }
  };
  size_t var_end() const { return neg_or_clause.var_end(); }
  size_t atom_count() const { return neg_or_clause.atom_count(); }
  Atom0 atom(size_t i) const { return neg_or_clause.atom(i).neg(); }
  OrClause0 neg() const { return neg_or_clause; }
private:
  explicit AndClause0(OrClause0 _neg_or_clause) : neg_or_clause(_neg_or_clause) {}
  OrClause0 neg_or_clause;
  friend AndClause0 OrClause0::neg() const;
};

AndClause0 OrClause0::neg() const { return AndClause0(*this); }

struct AndClause;

struct OrClause {
  u64 var_begin() const { return offset; }
  u64 var_end() const { return offset+cla.var_end(); }

  u64 atom_count() const { return cla.atom_count(); }
  Atom atom(size_t i) const { return Atom(offset,cla.atom(i)); }
  AndClause neg() const;
private:
  OrClause(u64 _offset, OrClause0 _cla) : offset(_offset), cla(_cla) {}
  u64 offset;
  OrClause0 cla;
};

struct AndClause {
  u64 var_begin() const { return neg_or_clause.var_begin(); }
  u64 var_end() const { return neg_or_clause.var_end(); }

  u64 atom_count() const { return neg_or_clause.atom_count(); }
  Atom atom(size_t i) const { return neg_or_clause.atom(i).neg(); }
  OrClause neg() const { return neg_or_clause; }
  private:
  explicit AndClause(OrClause _neg_or_clause) : neg_or_clause(_neg_or_clause) {}
  OrClause neg_or_clause;
  friend AndClause OrClause::neg() const;
};

AndClause OrClause::neg() const { return AndClause(*this); }

inline bool operator==(OrClause a, OrClause b) {
  if(a.atom_count()!=b.atom_count()) return 0;
  for(size_t i=a.atom_count(); i--;) if(a.atom(i)!=b.atom(i)) return 0;
  return 1;
}

inline bool operator!=(OrClause a, OrClause b) { return !(a==b); }
inline bool operator==(AndClause a, AndClause b) { return a.neg()==b.neg(); }
inline bool operator!=(AndClause a, AndClause b) { return !(a==b); }

}  // namespace tableau

#endif  // SYNTAX_CLAUSE_H_
