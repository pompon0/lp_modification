#ifndef INDEX_H_
#define INDEX_H_

#include <algorithm>
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/mgu.h"
#include "lazyparam_prover/ctx.h"

namespace tableau {

struct ClauseIndex {
  //TODO: eliminate clauses with unmatchable atoms
  struct OrClauseWithAtom { size_t i; DerOrClause cla; };
  NotAndForm form;
private:
  vec<vec<OrClauseWithAtom>> map;
public:
  ClauseIndex(const NotAndForm &f) : form(f) { FRAME("ClauseIndex");
    std::stable_sort(form.or_clauses.begin(),form.or_clauses.end(),
      [](const DerOrClause &a, const DerOrClause &b){ return a.cost()<b.cost(); });
    size_t id_offset = 0;
    for(auto &c : form.or_clauses) {
      c = c.set_id_offset(id_offset);
      id_offset += c.derived().atom_count();
    }
    map.assign(id_offset,{});
    vec<vec<OrClauseWithAtom>> preindex;
    for(const auto &_c : form.or_clauses) {
      auto c = _c.derived();
      for(size_t i=0; i<c.atom_count(); ++i) {
        auto h = Index::atom_hash(c.atom(i));
        if(h>=preindex.size()) preindex.resize(h+1);
        preindex[h].push_back({i,_c});
      }
    }
    //for(size_t i=0; i<preindex.size(); ++i) info("preindex[%].size() = %",i,preindex[i].size());
    
    Valuation val;
    auto s1 = val.snapshot();
    for(auto _c1 : form.or_clauses) {
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
          auto c2 = x.cla.shift(val.size()).derived();
          val.resize(c2.var_count());
          // unify
          if(c1.atom(i1).sign()==c2.atom(x.i).sign()) continue;
          if(!val.mgu(c1.atom(i1),c2.atom(x.i))) continue;
          map[c1.atom(i1).id()].push_back(x);
        }
      }
    }

    DEBUG {
      for(const auto &v : map) {
        size_t cost = 0;
        for(auto a : v) {
          if(a.cla.cost()<cost) error("cost not monotone");
          cost = a.cla.cost();
        }
      }
    }
  }

  const vec<OrClauseWithAtom>& operator()(Atom a) const {
    return map[a.id()];
  }
};

} // namespace tableau

#endif // INDEX_H_
