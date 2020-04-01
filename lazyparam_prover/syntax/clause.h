#ifndef SYNTAX_CLAUSE_H_
#define SYNTAX_CLAUSE_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct AndClause;

struct OrClause {
private:
  using VAR_RANGE = Field<VarRange>;
  using ATOMS = ArrayField<Atom,VAR_RANGE>;
  u8 *ptr;
  size_t offset;
  size_t id_offset;
  OrClause(u8 *_ptr, size_t _offset, size_t _id_offset) : ptr(_ptr), offset(_offset), id_offset(_id_offset) {}
public:
  OrClause(const vec<Atom> &_atoms) {
    Builder b(_atoms.size());
    for(size_t i = _atoms.size(); i--;) b.set_atom(i,_atoms[i]);
    *this = b.build();
  }

  VarRange var_range() const { return VAR_RANGE::ref(ptr)+offset; }
  size_t atom_count() const { return ATOMS::size(ptr); } 
  Atom atom(size_t i) const { return ATOMS::ref(ptr,i).shift(offset).set_id(id_offset+i); }
  OrClause shift(size_t _offset) const { return OrClause(ptr,offset+_offset,id_offset); }
  OrClause set_id_offset(size_t _id_offset) const { return OrClause(ptr,offset,_id_offset); }
  AndClause neg() const;

  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(size_t _atom_count) : ptr(ATOMS::alloc(_atom_count)) {
      VAR_RANGE::ref(ptr) = {0,0};
    }
    void set_atom(size_t i, Atom a) { FRAME("OrClause0.Builder.set_atom()");
      ATOMS::ref(ptr,i) = a;
      VAR_RANGE::ref(ptr) |= a.var_range();
    }
    OrClause build(){ return OrClause(ptr,0,0); }
  }; 
};

struct AndClause {
  struct Iso {
    using From = OrClause;
    using To = AndClause;
    From from(To c){ return c.neg(); }
    To to(From c){ return c.neg(); }
  };
  VarRange var_range() const { return neg_or_clause.var_range(); }
  size_t atom_count() const { return neg_or_clause.atom_count(); }
  Atom atom(size_t i) const { return neg_or_clause.atom(i).neg(); }
  OrClause neg() const { return neg_or_clause; }

  AndClause(vec<Atom> _atoms) : AndClause(OrClause([&_atoms]{
    for(auto &a : _atoms) a = a.neg();
    return _atoms;
  }())) {}
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
