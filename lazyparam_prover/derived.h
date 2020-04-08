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
    Builder b(1,0);
    b.set_cost(cost);
    b.set_source(0,cla);
    b.set_derived(cla);
    *this = b.build();
  }

  DerAndClause append_constraints(vec<OrderAtom> _constraints) const {
    Builder b(source_count(),constraint_count()+_constraints.size());
    b.set_cost(cost());
    b.set_derived(derived());
    for(size_t i=source_count(); i--;) b.set_source(i,source(i));
    for(size_t i=constraint_count(); i--;) b.set_constraint(i,constraint(i));
    for(size_t i=0; i<_constraints.size(); i++)
      b.set_constraint(constraint_count()+i,_constraints[i]);
    return b.build();
  }

  struct Builder {
    Builder(size_t sources_count, size_t constraints_count) :
      ptr(SOURCES::alloc(sources_count)),
      constraints_ptr(CONSTRAINTS::alloc(constraints_count)) { FRAME("DerAndClause::Builder(%,%)",sources_count,constraints_count);
      VAR_RANGE::ref(ptr) = {0,0};
    }

    Builder& set_cost(size_t cost){ COST::ref(ptr) = cost; return *this; }
    Builder& set_derived(AndClause derived){ FRAME("set_derived");
      DERIVED::ref(ptr) = derived;
      VAR_RANGE::ref(ptr) |= derived.var_range();
      return *this;
    }
    Builder& set_source(size_t i, AndClause source) { FRAME("set_source(%)",i);
      SOURCES::ref(ptr,i) = source;
      VAR_RANGE::ref(ptr) |= source.var_range();
      return *this;
    }
    Builder& set_constraint(size_t i, OrderAtom constraint) { FRAME("set_contraint(%)",i);
      CONSTRAINTS::ref(constraints_ptr,i) = constraint;
      VAR_RANGE::ref(ptr) |= constraint.var_range();
      return *this;
    }
    DerAndClause build(){ return DerAndClause(ptr,constraints_ptr,0,0); }
  private:
    u8 *ptr;
    u8 *constraints_ptr;
  };

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
