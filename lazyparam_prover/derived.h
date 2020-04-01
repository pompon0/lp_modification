#ifndef DERIVED_H_
#define DERIVED_H_

#include "lazyparam_prover/constraint.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"

namespace tableau {

struct DerAndClause;
struct DerOrClause;

struct DerOrClause {
  DerAndClause neg() const;
  DerOrClause set_id_offset(u64 _id_offset) const { return DerOrClause(ptr,constraints_ptr,offset,_id_offset); }
  DerOrClause shift(u64 _offset) const { return DerOrClause(ptr,constraints_ptr,offset+_offset,id_offset); }
  VarRange var_range() const { return VAR_RANGE::ref(ptr); }

  OrClause derived() const { return DERIVED::ref(ptr).shift(offset).set_id_offset(id_offset); }
  size_t cost() const { return COST::ref(ptr); }
  
  size_t source_count() const { return SOURCES::size(ptr); }
  OrClause source(size_t i) const { return SOURCES::ref(ptr,i).shift(offset); }
  
  size_t constraint_count() const { return CONSTRAINTS::size(constraints_ptr); }
  OrderAtom constraint(size_t i) const { return CONSTRAINTS::ref(ptr,i); }

  DerOrClause(size_t cost, OrClause cla) {
    Builder b(1,0);
    b.set_cost(cost);
    b.set_source(0,cla);
    b.set_derived(cla);
    *this = b.build();
  }

  DerOrClause append_constraints(vec<OrderAtom> _constraints) const {
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
      constraints_ptr(CONSTRAINTS::alloc(constraints_count)) {
      VAR_RANGE::ref(ptr) = {0,0};
    }

    Builder& set_cost(size_t cost){ COST::ref(ptr) = cost; return *this; }
    Builder& set_derived(OrClause derived){
      DERIVED::ref(ptr) = derived;
      VAR_RANGE::ref(ptr) |= derived.var_range();
      return *this;
    }
    Builder& set_source(size_t i, OrClause source) {
      SOURCES::ref(ptr,i) = source;
      VAR_RANGE::ref(ptr) |= source.var_range();
      return *this;
    }
    Builder& set_constraint(size_t i, OrderAtom constraint) {
      CONSTRAINTS::ref(constraints_ptr,i) = constraint;
      VAR_RANGE::ref(ptr) |= constraint.var_range();
      return *this;
    }
    DerOrClause build(){ return DerOrClause(ptr,constraints_ptr,0,0); }
  private:
    u8 *ptr;
    u8 *constraints_ptr;
  };

private:
  DerOrClause(u8 *_ptr, u8 *_constraints_ptr, size_t _offset, size_t _id_offset) 
    : ptr(_ptr), constraints_ptr(_constraints_ptr), offset(_offset), id_offset(_id_offset) {} 

  using COST = Field<size_t>;
  using DERIVED = Field<OrClause,COST>;
  using VAR_RANGE = Field<VarRange,DERIVED>;
  using SOURCES = ArrayField<OrClause,VAR_RANGE>;
  using CONSTRAINTS = ArrayField<OrderAtom>;
  u8 *ptr;
  u8 *constraints_ptr;
  size_t offset;
  size_t id_offset;
};

struct DerAndClause { 
  DerOrClause neg() const { return neg_or_clause; }
  DerAndClause set_id_offset(u64 _id_offset) const { return DerAndClause(neg_or_clause.set_id_offset(_id_offset)); }
  DerAndClause shift(size_t _offset) const { return DerAndClause(neg_or_clause.shift(_offset)); }
  VarRange var_range() const { return neg_or_clause.var_range(); }
  
  size_t cost() const { return neg_or_clause.cost(); }
  AndClause derived() const { return neg_or_clause.derived().neg(); }

  size_t source_count() const { return neg_or_clause.source_count(); }
  AndClause source(size_t i) const { return neg_or_clause.source(i).neg(); }

  size_t constraint_count() const { return neg_or_clause.constraint_count(); }
  OrderAtom constraint(size_t i) const { return neg_or_clause.constraint(i); }

private:
  explicit DerAndClause(DerOrClause _neg_or_clause) : neg_or_clause(_neg_or_clause) {}
  DerOrClause neg_or_clause;
  friend DerAndClause DerOrClause::neg() const;
};

DerAndClause DerOrClause::neg() const { return DerAndClause(*this); }

struct NotAndForm;
struct OrForm;

struct NotAndForm {
  vec<DerOrClause> or_clauses;
  NotAndForm(){}
  explicit NotAndForm(const OrForm &);
};

struct OrForm {
  vec<DerAndClause> and_clauses;
  OrForm(){}
  explicit OrForm(const NotAndForm &);
};

inline NotAndForm::NotAndForm(const OrForm &f) {
  for(const auto &c : f.and_clauses) or_clauses.push_back(c.neg());
}

inline OrForm::OrForm(const NotAndForm &f) {
  for(const auto &c : f.or_clauses) and_clauses.push_back(c.neg());
}

str show(const DerAndClause &cla) {
  vec<str> source;
  for(size_t i=0; i<cla.source_count(); ++i) source.push_back(show(cla.source(i)));
  return util::fmt("%   [%]",show(cla.derived()),util::join(", ",source));
}
str show(const DerOrClause &cla) { return show(cla.derived()); }

str show(const NotAndForm &f) {
  vec<str> clauses;
  for(auto c : f.or_clauses) clauses.push_back(show(c) + "\n");
  return util::join("",clauses);
}

str show(const OrForm &f) {
  vec<str> clauses;
  for(auto c : f.and_clauses) clauses.push_back(show(c) + "\n");
  return util::join("",clauses);
}

}  // namespace tableau

#endif  // DERIVED_H_
