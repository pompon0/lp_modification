#ifndef INDEX_H_
#define INDEX_H_

#include <algorithm>

#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/ctx.h"

namespace tableau {

struct ClauseIndex {
private:
  vec<DerOrClause> or_clauses;
  vec<bool> is_starting_clause;
  struct AtomClauseId { size_t atom_id, clause_id; };
  vec<vec<AtomClauseId>> map;
public:
  //TODO: eliminate clauses with unmatchable atoms
  struct OrClauseWithAtom { size_t i; DerOrClause cla; };

  struct Filter {
    Maybe<OrClauseWithAtom> next() { FRAME("ClauseIndex::Filter::next()");
      for(;next_id<atoms->size(); next_id++) {
        auto &a = (*atoms)[next_id];
        if(index->or_clauses[a.clause_id].cost()>cost_limit) return Maybe<OrClauseWithAtom>();
        if(starting_clause_id>a.clause_id && index->is_starting_clause[a.clause_id]) continue;
        next_id++;
        return Maybe<OrClauseWithAtom>({a.atom_id,index->or_clauses[a.clause_id]});
      }
      return Maybe<OrClauseWithAtom>(); 
    }
    Filter(
        size_t _cost_limit,
        const vec<AtomClauseId> *_atoms,
        size_t _starting_clause_id,
        const ClauseIndex *_index) :
      cost_limit(_cost_limit),
      next_id(0),
      atoms(_atoms),
      starting_clause_id(_starting_clause_id),
      index(_index) {}
  private:
    size_t cost_limit;
    size_t next_id;
    const vec<AtomClauseId> *atoms;
    size_t starting_clause_id;
    const ClauseIndex *index;
  };

  struct State {
    explicit State(const ClauseIndex *_index) : index(_index), next_starting_clause_id(0) {}

    Maybe<DerOrClause> next_starting_clause() {
      for(auto &i = next_starting_clause_id; i<index->or_clauses.size(); i++) {
        if(index->is_starting_clause[i])
          return Maybe<DerOrClause>(index->or_clauses[i++]);
      }
      return Maybe<DerOrClause>();
    }
    Filter get_matches(Atom atom, size_t cost_limit) {
      DEBUG if(next_starting_clause_id==0) error("next_starting_clause == 0");
      return Filter(
        cost_limit,
        &index->map[atom.id()],
        next_starting_clause_id-1,
        index);
    }

  private:
    const ClauseIndex *index;
    // Allows to filter out infeasible matches.
    // It is fixed for a given proof costruction (branch independent)
    size_t next_starting_clause_id; 
  };

  ClauseIndex(const NotAndForm &f) : or_clauses(f.or_clauses) { FRAME("ClauseIndex");
    std::stable_sort(or_clauses.begin(),or_clauses.end(),
      [](const DerOrClause &a, const DerOrClause &b){ return a.cost()<b.cost(); });
    is_starting_clause.resize(or_clauses.size());
    size_t id_offset = 0;
    // assign offsets to atoms
    // and mark the starting clauses (all negative ones)
    for(size_t i=or_clauses.size(); i--;) {
      auto &c = or_clauses[i];
      c = c.set_id_offset(id_offset);
      auto d = c.derived();
      id_offset += d.atom_count();
      bool starting = true;
      for(size_t j=d.atom_count(); j--;)
        starting &= !d.atom(j).sign();
      is_starting_clause[i] = starting;
    }
    map.assign(id_offset,{});
    vec<vec<AtomClauseId>> preindex;
    for(size_t i=0; i<or_clauses.size(); ++i) {
      auto c = or_clauses[i].derived();
      for(size_t j=0; j<c.atom_count(); ++j){
        auto h = Index::atom_hash(c.atom(j));
        if(h>=preindex.size()) preindex.resize(h+1);
        preindex[h].push_back({j,i});
      }
    }
    //for(size_t i=0; i<preindex.size(); ++i) info("preindex[%].size() = %",i,preindex[i].size());
    
    Valuation val;
    auto s1 = val.snapshot();
    for(auto _c1 : or_clauses) {
      // allocate c1
      val.rewind(s1);
      auto c1 = _c1.shift(val.size()).derived();
      val.resize(c1.var_count());
      auto s2 = val.snapshot(); 
      for(size_t i1=0; i1<c1.atom_count(); ++i1) {
        auto h = Index::atom_hash(c1.atom(i1));
        if((h^1)>=preindex.size()) continue;
        for(auto x : preindex[h^1]) {
          // allocate c2
          val.rewind(s2);
          auto c2 = or_clauses[x.clause_id].shift(val.size()).derived();
          val.resize(c2.var_count());
          // unify
          if(c1.atom(i1).sign()==c2.atom(x.atom_id).sign()) continue;
          if(!val.mgu(c1.atom(i1),c2.atom(x.atom_id))) continue;
          map[c1.atom(i1).id()].push_back(x);
        }
      }
    }

    DEBUG {
      for(const auto &v : map) {
        size_t cost = 0;
        for(auto a : v) {
          if(or_clauses[a.clause_id].cost()<cost) error("cost not monotone");
          cost = or_clauses[a.clause_id].cost();
        }
      }
    }
  }
};

} // namespace tableau

#endif // INDEX_H_
