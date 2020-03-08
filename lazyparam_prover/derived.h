#ifndef DERIVED_H_
#define DERIVED_H_

#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/pred_format.h"

namespace tableau {

struct DerAndClause;
struct DerOrClause;

struct DerOrClause {
  DerAndClause neg() const;
  DerOrClause set_id_offset(u64 _id_offset) const {
    DEBUG if(id_offset_!=0) error("id_offset = %, want %",id_offset_,0);
    return DerOrClause(cost_,var_offset_,_id_offset,derived_,source_,constraints_);
  }
  OrClause derived() const { return derived_.shift(var_offset_).set_id_offset(id_offset_); }
  size_t cost() const { return cost_; }
  
  List<OrClause> source_list() const {
    List<OrClause> source2;
    for(auto l=source_; !l.empty(); l = l.tail()) {
      source2 += l.head().shift(var_offset_);
    }
    return source2;
  }
  
  List<Constraint> constraints() const {
    List<Constraint> constraints2;
    for(auto c = constraints_; !c.empty(); c = c.tail()) {
      constraints2 += c.head().shift(var_offset_);
    }
    return constraints2;
  }
  
  vec<OrClause> source() const {
    vec<OrClause> s; for(auto l=source_; !l.empty(); l = l.tail()) s.push_back(l.head().shift(var_offset_));
    return s;
  }

private:
  friend NoOffset<DerOrClause>;
  DerOrClause(size_t _cost, size_t _var_offset, size_t _id_offset, NoOffset<OrClause> _derived, List<NoOffset<OrClause>> _source, List<Constraint> _constraints)
    : cost_(_cost), var_offset_(_var_offset), id_offset_(_id_offset), derived_(_derived), source_(_source), constraints_(_constraints) {}
  size_t cost_;
  size_t var_offset_;
  size_t id_offset_;
  NoOffset<OrClause> derived_;
  List<NoOffset<OrClause>> source_;
  List<Constraint> constraints_;
};

template<> struct NoOffset<DerOrClause> : DerOrClause {
  NoOffset(size_t _cost, NoOffset<OrClause> _derived, List<NoOffset<OrClause>> _source, List<Constraint> _constraints)
    : DerOrClause(_cost,0,0,_derived,_source,_constraints) {} 
  NoOffset(size_t _cost, NoOffset<OrClause> cla)
    : NoOffset<DerOrClause>(_cost,cla,List<NoOffset<OrClause>>(cla),List<Constraint>()) {}
  DerOrClause shift(NoOffset<DerOrClause> cla, size_t _var_offset) const {
    return DerOrClause(cla.cost_,_var_offset,cla.id_offset_,cla.derived_,cla.source_,cla.constraints_);
  }
};

struct DerAndClause { 
  size_t cost() const { return neg_or_clause.cost(); }
  AndClause derived() const { return neg_or_clause.derived().neg(); }
  DerAndClause set_id_offset(u64 _id_offset) { return DerAndClause(neg_or_clause.set_id_offset(_id_offset)); }
  DerOrClause neg() const { return neg_or_clause; }
  ListA<AndClause::Iso> source_list() const { return ListA<AndClause::Iso>(neg_or_clause.source_list()); }
  List<Constraint> constraints() const { return neg_or_clause.constraints(); }
private:
  explicit DerAndClause(DerOrClause _neg_or_clause) : neg_or_clause(_neg_or_clause) {}
  DerOrClause neg_or_clause;
  friend DerAndClause DerOrClause::neg() const;
};

DerAndClause DerOrClause::neg() const { return DerAndClause(*this); }



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
  for(auto c : cla.source_list().to_vec()) source.push_back(show(c));
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
