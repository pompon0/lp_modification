#ifndef INDEX_H_
#define INDEX_H_

#include <algorithm>
#include "lazyparam_prover/eq_axioms.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/mgu.h"
#include "utils/ctx.h"

namespace tableau {

struct ClauseIndex {
private:
  vec<DerAndClause> and_clauses;
  vec<bool> is_starting_clause;
  struct AtomClauseId { size_t atom_id, clause_id; };
  vec<vec<AtomClauseId>> sets;
  vec<AtomClauseId> all;
  vec<size_t> map;
public:
  ~ClauseIndex(){}
  //TODO: eliminate clauses with unmatchable atoms
  struct AndClauseWithAtom { size_t i; DerAndClause cla; };

  struct Filter {
    INL memory::Maybe<AndClauseWithAtom> next() { FRAME("ClauseIndex::Filter::next()");
      for(;next_id<atoms->size(); next_id++) {
        auto &a = (*atoms)[next_id];
        if(index->and_clauses[a.clause_id].cost()>cost_limit) return memory::Maybe<AndClauseWithAtom>();
        if(starting_clause_id>a.clause_id && index->is_starting_clause[a.clause_id]) continue;
        next_id++;
        return memory::Maybe<AndClauseWithAtom>({a.atom_id,index->and_clauses[a.clause_id]});
      }
      return memory::Maybe<AndClauseWithAtom>(); 
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

    memory::Maybe<DerAndClause> next_starting_clause() {
      for(auto &i = next_starting_clause_id; i<index->and_clauses.size(); i++) {
        if(index->is_starting_clause[i])
          return memory::just(index->and_clauses[i++]);
      }
      return memory::nothing();
    }
    // gets all atoms which are matchable with atom.neg().
    // WARNING: Use only if atom comes from the fixed initial set.
    // TODO: mark the atoms, so that upon change, this function raises an exception.
    Filter get_matches(Atom atom, size_t cost_limit) {
      DEBUG if(next_starting_clause_id==0) error("next_starting_clause == 0");
      return Filter(
        cost_limit,
        &index->sets[index->map[atom.id()]],
        next_starting_clause_id-1,
        index);
    }
    // gets all atoms with a given pred and sign.
    Filter get_matches(size_t pred, bool sign, size_t cost_limit) {
      DEBUG if(next_starting_clause_id==0) error("next_starting_clause == 0");
      return Filter(
        cost_limit,
        &index->sets[Index::atom_hash(pred,sign)],
        next_starting_clause_id-1,
        index);
    }
    // get all atoms.
    Filter get_all(size_t cost_limit) {
      DEBUG if(next_starting_clause_id==0) error("next_starting_clause == 0");
      return Filter(
        cost_limit,
        &index->all,
        next_starting_clause_id-1,
        index);
    }
  private:
    const ClauseIndex *index;
    // Allows to filter out infeasible matches.
    // It is fixed for a given proof costruction (branch independent)
    size_t next_starting_clause_id; 
  };

  ClauseIndex(const OrForm &f) : and_clauses(f.and_clauses) { FRAME("ClauseIndex");
    std::stable_sort(and_clauses.begin(),and_clauses.end(),
      [](const DerAndClause &a, const DerAndClause &b){ return a.cost()<b.cost(); });
    is_starting_clause.resize(and_clauses.size());
    size_t id_offset = 0;
    // assign offsets to atoms
    // and mark the starting clauses (all positive ones)
    for(size_t i=and_clauses.size(); i--;) {
      auto &c = and_clauses[i];
      c = c.set_id_offset(id_offset);
      auto d = c.derived();
      id_offset += d.atom_count();
      bool starting = true;
      for(size_t j=d.atom_count(); j--;)
        starting &= d.atom(j).sign();
      is_starting_clause[i] = starting;
    }
    map.assign(id_offset,{});
    auto &preindex = sets;
    for(size_t i=0; i<and_clauses.size(); ++i) {
      auto c = and_clauses[i].derived();
      for(size_t j=0; j<c.atom_count(); ++j){
        auto h = Index::atom_hash(c.atom(j));
        if((h|1)>=preindex.size()) preindex.resize((h|1)+1);
        preindex[h].push_back({j,i});
        all.push_back({j,i});
      }
    }
    //for(size_t i=0; i<preindex.size(); ++i) info("preindex[%].size() = %",i,preindex[i].size());
    memory::Alloc A;
    Valuation val;
    auto s1 = val.save();
    for(auto _c1 : and_clauses) {
      // allocate c1
      val.restore(s1);
      auto c1 = val.allocate(_c1).derived();
      auto s2 = val.save(); 
      for(size_t i1=0; i1<c1.atom_count(); ++i1) {
        DEBUG info("[%] % ::>",c1.atom(i1).id(),show(c1.atom(i1)));
        auto h = Index::atom_hash(c1.atom(i1));
        if(preindex[h^1].size()>100) {
          map[c1.atom(i1).id()] = h^1;
        } else {
          map[c1.atom(i1).id()] = sets.size();
          sets.push_back({});
          for(auto x : preindex[h^1]) {
            // allocate c2
            val.restore(s2);
            auto c2 = val.allocate(and_clauses[x.clause_id]).derived();
            // unify
            if(c1.atom(i1).sign()==c2.atom(x.atom_id).sign()) continue;
            if(!val.mgu(c1.atom(i1),c2.atom(x.atom_id))) continue;
            sets.back().push_back(x);
          }
        }

        DEBUG for(auto x : sets[map[c1.atom(i1).id()]]) info("\t[%] %",x.atom_id,show(and_clauses[x.clause_id]));
      }
    }

    DEBUG {
      for(const auto &v : sets) {
        size_t cost = 0;
        for(auto a : v) {
          if(and_clauses[a.clause_id].cost()<cost) error("cost not monotone");
          cost = and_clauses[a.clause_id].cost();
        }
      }
    }
  }
};

} // namespace tableau

#endif // INDEX_H_
