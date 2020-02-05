#ifndef DERIVED_H_
#define DERIVED_H_

#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/pred_format.h"

namespace tableau {

struct Constraint {
  // ignores sign
  static Constraint neq(Atom l, Atom r) {
    if(l.pred()!=r.pred()) return Constraint{TRUE};
    DEBUG if(l.arg_count()!=r.arg_count()) error("l.arg_count() = %, r.arg_count() = %",show(l),show(r));
    List<Constraint::Pair> p;
    for(size_t i=l.arg_count(); i--;) p += {l.arg(i),r.arg(i)};
    return Constraint{NEQ,p};
  }

  static Constraint neq(Term l, Term r) {
    return Constraint{NEQ,List<Pair>(Pair{l,r})};
  }

  static Constraint lt(Term l, Term r) {
    return Constraint{LT,List<Pair>(Pair{l,r})};
  }

  static Constraint le(Term l, Term r) {
    return Constraint{LE,List<Pair>(Pair{l,r})};
  }

  enum Type { NEQ, LT, LE, TRUE };
  Type type;
  struct Pair { Term l,r; };
  List<Pair> or_;
  
  Constraint shift(size_t var_offset) const {
    List<Pair> or2;
    for(auto p = or_; !p.empty(); p = p.tail()) {
      auto h = p.head();
      or2 += Pair{
        Term(h.l.ptr,var_offset),
        Term(h.r.ptr,var_offset),
      };
    }
    return Constraint{type,or2};
  }
};

struct DerAndClause;
struct DerOrClause;

struct DerAndClause {
  DerAndClause(){}
  explicit DerAndClause(size_t _cost, AndClause cla) : cost(_cost), derived(cla), source{cla} {}

  DerOrClause neg() const;
  
  size_t cost = 0;
  AndClause derived;
  vec<AndClause> source;
  vec<Constraint> constraints;
};

struct DerOrClause {
  DerOrClause(size_t _cost, OrClause cla) : DerOrClause(_cost,cla,List<OrClause>(cla),List<Constraint>()) {}
  DerOrClause(size_t _cost, OrClause _derived, List<OrClause> _source, List<Constraint> _constraints)
    : DerOrClause(_cost,0,0,_derived,_source,_constraints) {}
  DerAndClause neg() const;
  DerOrClause shift(size_t _var_offset) const {
    DEBUG if(var_offset_) error("offset = %, want %",var_offset_,0);
    return DerOrClause(cost_,_var_offset,id_offset_,derived_,source_,constraints_);
  }
  DerOrClause set_id_offset(u64 _id_offset) {
    DEBUG if(id_offset_!=0) error("id_offset = %, want %",id_offset_,0);
    return DerOrClause(cost_,var_offset_,_id_offset,derived_,source_,constraints_);
  }
  OrClause derived() const { return derived_.shift(var_offset_).set_id_offset(id_offset_); }
  size_t cost() const { return cost_; }
  
  List<OrClause> source_list() {
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
  DerOrClause(size_t _cost, size_t _var_offset, size_t _id_offset, OrClause _derived, List<OrClause> _source, List<Constraint> _constraints)
    : cost_(_cost), var_offset_(_var_offset), id_offset_(_id_offset), derived_(_derived), source_(_source), constraints_(_constraints) {}
  size_t cost_;
  size_t var_offset_;
  size_t id_offset_;
  OrClause derived_;
  List<OrClause> source_;
  List<Constraint> constraints_;
};

struct OrForm {
  vec<DerAndClause> and_clauses;
  OrForm(){}
  explicit OrForm(const NotAndForm &);
};

struct NotAndForm {
  vec<DerOrClause> or_clauses;
  NotAndForm(){}
  explicit NotAndForm(const OrForm &);
};

inline NotAndForm::NotAndForm(const OrForm &f) {
  for(const auto &c : f.and_clauses) or_clauses.push_back(c.neg());
}

inline OrForm::OrForm(const NotAndForm &f) {
  for(const auto &c : f.or_clauses) and_clauses.push_back(c.neg());
}



str show(const DerAndClause &cla) {
  vec<str> source;
  for(auto c : cla.source) source.push_back(show(c));
  return util::fmt("%   [%]",show(cla.derived),util::join(", ",source));
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

DerOrClause DerAndClause::neg() const { FRAME("neg(%)",show(*this));
  List<OrClause> source_neg;
  List<Constraint> constraints_list;
  for(const auto &c : source) source_neg += c.neg();
  for(const auto &c : constraints) constraints_list += c;
  return DerOrClause(cost,derived.neg(),source_neg,constraints_list);
}

DerAndClause DerOrClause::neg() const { FRAME("neg(%)",show(*this));
  DerAndClause cla;
  cla.cost = cost();
  cla.derived = derived().neg();
  for(auto c : source()) {
    cla.source.push_back(c.neg());
  }
  for(auto c = constraints_; !c.empty(); c = c.tail()) {
    cla.constraints.push_back(c.head());
  }
  return cla;
}

}  // namespace tableau

#endif  // DERIVED_H_
