#ifndef SYNTAX_CLAUSE_H_
#define SYNTAX_CLAUSE_H_

#include "lazyparam_prover/syntax/atom.h"
#include "utils/types.h"
#include "lazyparam_prover/memory/layout.h"
#include "lazyparam_prover/memory/stack.h"

namespace tableau {

struct OrClause;

struct AndClause {
private:
  using VAR_RANGE = memory::Field<VarRange>;
  using ATOMS = memory::ArrayField<Atom,VAR_RANGE>;
  u8 *ptr;
  size_t offset;
  size_t id_offset;
  INL AndClause(u8 *_ptr, size_t _offset, size_t _id_offset) : ptr(_ptr), offset(_offset), id_offset(_id_offset) {}
public:
  template<typename ...Atoms> INL static AndClause make(memory::Alloc &a, Atoms... atoms) {
    Builder b(a,sizeof...(Atoms));
    size_t i=0; (b.set_atom(i++,atoms),...);
    return b.build();
  }

  INL VarRange var_range() const { return VAR_RANGE::ref(ptr)+offset; }
  INL size_t atom_count() const { return ATOMS::size(ptr); } 
  INL Atom atom(size_t i) const { return ATOMS::ref(ptr,i).shift(offset).set_id(id_offset+i); }
  INL AndClause shift(size_t _offset) const { return AndClause(ptr,offset+_offset,id_offset); }
  INL AndClause set_id_offset(size_t _id_offset) const { return AndClause(ptr,offset,_id_offset); }
  INL OrClause neg() const;

  struct Builder {
  private:
    u8 *ptr;
  public:
    INL Builder(memory::Alloc &A, size_t _atom_count) : ptr(ATOMS::alloc(A,_atom_count)) {
      VAR_RANGE::ref(ptr) = {0,0};
    }
    INL void set_atom(size_t i, Atom a) { FRAME("AndClause0.Builder.set_atom()");
      ATOMS::ref(ptr,i) = a;
      VAR_RANGE::ref(ptr) |= a.var_range();
    }
    INL AndClause build(){ return AndClause(ptr,0,0); }
  }; 
};

struct OrClause {
  struct Iso {
    using From = AndClause;
    using To = OrClause;
    From from(To c){ return c.neg(); }
    To to(From c){ return c.neg(); }
  };
  INL VarRange var_range() const { return neg().var_range(); }
  INL size_t atom_count() const { return neg().atom_count(); }
  INL Atom atom(size_t i) const { return neg().atom(i).neg(); }
  INL OrClause shift(size_t _offset) const { return neg().shift(_offset).neg(); }
  INL OrClause set_id_offset(size_t _id_offset) const { return neg().set_id_offset(_id_offset).neg(); }
  INL AndClause neg() const { return neg_and_clause; }

  template<typename ...Atoms> INL static OrClause make(Atoms ...atoms) {
    return OrClause(AndClause::make(atoms.neg()...));
  }
private:
  INL explicit OrClause(AndClause _neg_and_clause) : neg_and_clause(_neg_and_clause) {}
  AndClause neg_and_clause;
  friend OrClause AndClause::neg() const;
};

INL inline OrClause AndClause::neg() const { return OrClause(*this); }

INL inline bool operator==(AndClause a, AndClause b) {
  if(a.atom_count()!=b.atom_count()) return 0;
  for(size_t i=a.atom_count(); i--;) if(a.atom(i)!=b.atom(i)) return 0;
  return 1;
}

INL inline bool operator!=(AndClause a, AndClause b) { return !(a==b); }
INL inline bool operator==(OrClause a, OrClause b) { return a.neg()==b.neg(); }
INL inline bool operator!=(OrClause a, OrClause b) { return !(a==b); }

}  // namespace tableau

#endif  // SYNTAX_CLAUSE_H_
