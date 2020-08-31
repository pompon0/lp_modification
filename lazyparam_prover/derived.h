#ifndef DERIVED_H_
#define DERIVED_H_

#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/stack.h"

namespace tableau {

struct DerAndClause;

struct DerAndClause {
  INL DerAndClause set_id_offset(u64 _id_offset) const { return DerAndClause(ptr,constraints_ptr,offset,_id_offset); }
  INL DerAndClause shift(u64 _offset) const { return DerAndClause(ptr,constraints_ptr,offset+_offset,id_offset); }
  INL VarRange var_range() const { return VAR_RANGE::ref(ptr); }

  INL AndClause derived() const { return DERIVED::ref(ptr).shift(offset).set_id_offset(id_offset); }
  INL size_t cost() const { return COST::ref(ptr); }
  
  INL size_t source_count() const { return SOURCES::size(ptr); }
  INL AndClause source(size_t i) const { return SOURCES::ref(ptr,i).shift(offset); }
  
  INL size_t constraint_count() const { return CONSTRAINTS::size(constraints_ptr); }
  INL OrderAtom constraint(size_t i) const { return CONSTRAINTS::ref(constraints_ptr,i).shift(offset); }

  INL DerAndClause(memory::Alloc &A, size_t cost, AndClause cla) {
    Builder b(A);
    b.cost = cost;
    b.derived = cla;
    b.sources.push_back(cla);
    *this = b.build(A);
  }

  struct Builder {
  private:
  public:
    INL ~Builder() = default;

    size_t offset = 0;
    size_t id_offset = 0;
    size_t cost = 0;
    AndClause derived;
    vec<AndClause> sources;
    vec<OrderAtom> constraints;

    Builder(memory::Alloc &A) : derived(AndClause::make(A)) {}
    INL DerAndClause build(memory::Alloc &A) {
      auto ptr = SOURCES::alloc(A,sources.size());
      auto constraints_ptr = CONSTRAINTS::alloc(A,constraints.size());
      VarRange var_range{0,0};
      var_range |= derived.var_range();
      for(auto &s : sources) var_range |= s.var_range();
      for(auto &c : constraints) var_range |= c.var_range();
      VAR_RANGE::ref(ptr) = var_range;
      COST::ref(ptr) = cost;
      DERIVED::ref(ptr) = derived;
      for(size_t i=0; i<sources.size(); i++) SOURCES::ref(ptr,i) = sources[i];
      for(size_t i=0; i<constraints.size(); i++) CONSTRAINTS::ref(constraints_ptr,i) = constraints[i];
      return DerAndClause(ptr,constraints_ptr,offset,id_offset);
    }
  };

  INL Builder to_builder(memory::Alloc &A) {
    Builder b(A);
    b.offset = offset;
    b.id_offset = id_offset;
    b.cost = cost();
    b.derived = derived();
    for(size_t i=source_count(); i--;) b.sources.push_back(source(i));
    for(size_t i=constraint_count(); i--;) b.constraints.push_back(constraint(i));
    return b;
  }

  INL friend bool operator==(const DerAndClause &a, const DerAndClause &b) {
    bool ok = a.cost()==b.cost();
    ok &= a.derived()==b.derived();
    ok &= a.source_count()==b.source_count();
    ok &= a.constraint_count()==b.constraint_count();
    if(!ok) return false;
    for(size_t i=a.source_count(); i--;) ok &= a.source(i)==b.source(i);
    for(size_t i=a.constraint_count(); i--;) ok &= a.constraint(i)==b.constraint(i);
    return ok;
  }

private:
  INL DerAndClause(u8 *_ptr, u8 *_constraints_ptr, size_t _offset, size_t _id_offset) 
    : ptr(_ptr), constraints_ptr(_constraints_ptr), offset(_offset), id_offset(_id_offset) {} 

  using COST = Field<size_t>;
  using DERIVED = Field<AndClause,COST>;
  using VAR_RANGE = Field<VarRange,DERIVED>;
  using SOURCES = ArrayField<AndClause,VAR_RANGE>;
  using CONSTRAINTS = ArrayField<OrderAtom>;
  u8 *ptr;
  u8 *constraints_ptr;
  size_t offset;
  size_t id_offset;
};

struct OrForm {
  vec<DerAndClause> and_clauses;
};

str show(const DerAndClause &cla) {
  vec<str> source;
  for(size_t i=0; i<cla.source_count(); ++i) source.push_back(show(cla.source(i)));
  vec<str> constraints;
  for(size_t i=0; i<cla.constraint_count(); ++i) constraints.push_back(show(cla.constraint(i)));
  return util::fmt("%   [%] [%]",show(cla.derived()),util::join(", ",constraints),util::join(", ",source));
}

str show(const OrForm &f) {
  vec<str> clauses;
  for(auto c : f.and_clauses) clauses.push_back(show(c) + "\n");
  return util::join("",clauses);
}

}  // namespace tableau

#endif  // DERIVED_H_
