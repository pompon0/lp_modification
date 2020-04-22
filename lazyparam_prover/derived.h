#ifndef DERIVED_H_
#define DERIVED_H_

#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"

namespace tableau {

struct DerAndClause;

struct DerAndClause {
  DerAndClause set_id_offset(u64 _id_offset) const { return DerAndClause(ptr,constraints_ptr,offset,_id_offset); }
  DerAndClause shift(u64 _offset) const { return DerAndClause(ptr,constraints_ptr,offset+_offset,id_offset); }
  VarRange var_range() const { return VAR_RANGE::ref(ptr); }

  AndClause derived() const { return DERIVED::ref(ptr).shift(offset).set_id_offset(id_offset); }
  size_t cost() const { return COST::ref(ptr); }
  
  size_t source_count() const { return SOURCES::size(ptr); }
  AndClause source(size_t i) const { return SOURCES::ref(ptr,i).shift(offset); }
  
  size_t constraint_count() const { return CONSTRAINTS::size(constraints_ptr); }
  OrderAtom constraint(size_t i) const { return CONSTRAINTS::ref(constraints_ptr,i); }

  DerAndClause(size_t cost, AndClause cla) {
    Builder b;
    b.cost = cost;
    b.derived = cla;
    b.sources.push_back(cla);
    *this = b.build();
  }

  /*DerAndClause append_constraints(vec<OrderAtom> _constraints) const {
    Builder b(source_count(),constraint_count()+_constraints.size());
    b.set_cost(cost());
    b.set_derived(derived());
    for(size_t i=source_count(); i--;) b.set_source(i,source(i));
    for(size_t i=constraint_count(); i--;) b.set_constraint(i,constraint(i));
    for(size_t i=0; i<_constraints.size(); i++)
      b.set_constraint(constraint_count()+i,_constraints[i]);
    return b.build();
  }*/

  struct Builder {
    size_t offset = 0;
    size_t id_offset = 0;
    size_t cost = 0;
    AndClause derived = emptyAndClause();
    vec<AndClause> sources;
    vec<OrderAtom> constraints;

    DerAndClause build() {
      auto ptr = SOURCES::alloc(sources.size());
      auto constraints_ptr = CONSTRAINTS::alloc(constraints.size());
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

  Builder to_builder() {
    Builder b;
    b.offset = offset;
    b.id_offset = id_offset;
    b.cost = cost();
    b.derived = derived();
    for(size_t i=source_count(); i--;) b.sources.push_back(source(i));
    for(size_t i=constraint_count(); i--;) b.constraints.push_back(constraint(i));
    return b;
  }

private:
  DerAndClause(u8 *_ptr, u8 *_constraints_ptr, size_t _offset, size_t _id_offset) 
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
  return util::fmt("%   [%]",show(cla.derived()),util::join(", ",source));
}

str show(const OrForm &f) {
  vec<str> clauses;
  for(auto c : f.and_clauses) clauses.push_back(show(c) + "\n");
  return util::join("",clauses);
}

}  // namespace tableau

#endif  // DERIVED_H_
