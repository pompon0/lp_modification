#ifndef SYNTAX_CLAUSE_H_
#define SYNTAX_CLAUSE_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/types.h"
#include "lazyparam_prover/memory/layout.h"

namespace tableau {

struct OrClause;

struct AndClause {
private:
  using VAR_RANGE = Field<VarRange>;
  using ATOMS = ArrayField<Atom,VAR_RANGE>;
  u8 *ptr;
  size_t offset;
  size_t id_offset;
  AndClause(u8 *_ptr, size_t _offset, size_t _id_offset) : ptr(_ptr), offset(_offset), id_offset(_id_offset) {}
public:
  template<typename ...Atoms> static AndClause make(Atoms... atoms) {
    if(sizeof...(Atoms)==0) {
      static Builder b(0);
      return b.build();
    }
    Builder b(sizeof...(Atoms));
    size_t i=0; (b.set_atom(i++,atoms),...);
    return b.build();
  }

  VarRange var_range() const { return VAR_RANGE::ref(ptr)+offset; }
  size_t atom_count() const { return ATOMS::size(ptr); } 
  Atom atom(size_t i) const { return ATOMS::ref(ptr,i).shift(offset).set_id(id_offset+i); }
  AndClause shift(size_t _offset) const { return AndClause(ptr,offset+_offset,id_offset); }
  AndClause set_id_offset(size_t _id_offset) const { return AndClause(ptr,offset,_id_offset); }
  OrClause neg() const;

  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(size_t _atom_count) : ptr(ATOMS::alloc(_atom_count)) {
      VAR_RANGE::ref(ptr) = {0,0};
    }
    void set_atom(size_t i, Atom a) { FRAME("AndClause0.Builder.set_atom()");
      ATOMS::ref(ptr,i) = a;
      VAR_RANGE::ref(ptr) |= a.var_range();
    }
    AndClause build(){ return AndClause(ptr,0,0); }
  }; 
};

struct OrClause {
  struct Iso {
    using From = AndClause;
    using To = OrClause;
    From from(To c){ return c.neg(); }
    To to(From c){ return c.neg(); }
  };
  VarRange var_range() const { return neg().var_range(); }
  size_t atom_count() const { return neg().atom_count(); }
  Atom atom(size_t i) const { return neg().atom(i).neg(); }
  OrClause shift(size_t _offset) const { return neg().shift(_offset).neg(); }
  OrClause set_id_offset(size_t _id_offset) const { return neg().set_id_offset(_id_offset).neg(); }
  AndClause neg() const { return neg_and_clause; }

  template<typename ...Atoms> static OrClause make(Atoms ...atoms) {
    return OrClause(AndClause::make(atoms.neg()...));
  }
private:
  explicit OrClause(AndClause _neg_and_clause) : neg_and_clause(_neg_and_clause) {}
  AndClause neg_and_clause;
  friend OrClause AndClause::neg() const;
};

inline OrClause AndClause::neg() const { return OrClause(*this); }

inline bool operator==(AndClause a, AndClause b) {
  if(a.atom_count()!=b.atom_count()) return 0;
  for(size_t i=a.atom_count(); i--;) if(a.atom(i)!=b.atom(i)) return 0;
  return 1;
}

inline bool operator!=(AndClause a, AndClause b) { return !(a==b); }
inline bool operator==(OrClause a, OrClause b) { return a.neg()==b.neg(); }
inline bool operator!=(OrClause a, OrClause b) { return !(a==b); }

}  // namespace tableau

#endif  // SYNTAX_CLAUSE_H_
