#ifndef INDEX_H_
#define INDEX_H_

#include <algorithm>
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/mgu.h"

namespace tableau {

struct ClauseIndex {
  struct OrClauseWithAtom { size_t i; DerOrClause cla; };
  NotAndForm form;
private:
  vec<vec<OrClauseWithAtom>> map;
public:
  ClauseIndex(const NotAndForm &f) : form(f) {
    std::stable_sort(form.or_clauses.begin(),form.or_clauses.end(),
      [](const DerOrClause &a, const DerOrClause &b){ return a.cost()<b.cost(); });
    size_t id_offset = 0;
    for(auto &c : form.or_clauses) {
      c = c.set_id_offset(id_offset);
      id_offset += c.derived().atom_count();
    }
    map.assign(id_offset,{});
    
    Valuation val;
    auto s1 = val.snapshot();
    for(auto _c1 : form.or_clauses) {
      // allocate c1
      val.rewind(s1);
      auto c1 = _c1.shift(val.size()).derived();
      val.resize(c1.var_count());
      auto s2 = val.snapshot(); 
      for(auto _c2 : form.or_clauses) {
        // allocate c2
        val.rewind(s2);
        auto c2 = _c2.shift(val.size()).derived();
        val.resize(c2.var_count());
        auto s3 = val.snapshot();
        for(size_t i1=0; i1<c1.atom_count(); ++i1) {
          for(size_t i2=0; i2<c2.atom_count(); ++i2) {
            val.rewind(s3);
            // unify
            if(c1.atom(i1).sign()==c2.atom(i2).sign()) continue;
            if(!val.mgu(c1.atom(i1),c2.atom(i2))) continue;
            map[c1.atom(i1).id()].push_back({i2,_c2});
          }
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
