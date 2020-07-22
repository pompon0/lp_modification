#ifndef FF_PROVER_H
#define FF_PROVER_H

#include <iostream>
#include <cstdint>
#include <vector>
#include <tuple>
#include <unordered_map>
#include <cassert>
#include <functional>

#include "memory/stack.h"
#include "memory/list.h"

template<class T> ostream& operator<<(ostream& os, const tvec<T>& v) {
  os << '[';
  for (uint32_t i = 0; i < v.parent.size(); ++i)
    cout << v.parent[i] << ' ';
  cout << "] " << v.top;
  return os;
}

struct trm {
  trm(int64_t f, vec<int64_t> args) a(args.size(),0) {
    for(size_t i=0; i<args.size(); ++i) a[i] = trm_hash[args[i]];
  }
  const int64_t f;
  vec<trm*> a;

  ostream& print_raw_tm(ostream& os) const;
  ostream& print_raw_lit(ostream& os) const;
  
  trm& operator=(const trm&)=delete;
  trm(trm const&)=delete;
};

struct trmo { trm* t = 0; uint32_t o = 0; };

struct contra {
  trm* lit1;
  const int64_t cno;
  const uint32_t vars;
  const uint32_t rest_len;
  vec<trm*> rest;
  contra(const contra& o) = delete;
  contra& operator=(const contra&) = delete;
};

ostream& operator<<(ostream& os, const contra& c);

struct problem {
  vector<contra> contras;
  unordered_multimap<int64_t, int32_t> lit_contr; // (negated) literal ptr -> contr_id
  problem(const value v) { init(v); }
  problem(bool conj, int def, bool contentnames, int reo, const char* fname);
  problem& operator=(const problem&)=delete;
  problem(problem const&)=delete;
private:
  void init(const value v) {
    for (value vcon = fourth(v); vcon != Val_emptylist; vcon = snd(vcon)) {
      contras.emplace_back(fst(vcon));
      lit_contr.emplace(-lit_hash[Long_val(fst(third(fst(vcon))))]->f, contras.size() - 1);
    }
  }
}
};

ostream& operator<<(ostream& os, const problem& p);

struct trmo2 { trmo t1,t2; };

struct Action {
  enum Type {lem = 0, red = 1, ext = 2} type;
  uint32_t no;
};

struct goal {
  vec<trm*> cl;
  uint32_t cl_start;
  uint32_t cl_off;
  
  uint32_t path_len;
  uint32_t lem_len;
  uint32_t nlem_off;
  trm* nlem;
};

struct backpoint {
  List<trmo> path;
  List<trmo> lem;
  List<goal> goals;
  
  vec<trm*> cl;
  uint32_t cl_start;
  uint32_t cl_off;
  uint32_t subst_hist_len;
  uint32_t next_var;
  
  backpoint() = delete;
  backpoint& operator=(const backpoint&) = delete;
};

struct prover {
  ptr<memory::Stack> mem;
  explicit prover(problem &_p) : prob(_p) { save(); }
  
  problem &prob;
  vec<trmo> subst;
  vec<uint32_t> subst_hist;
  vec<trmo2> ts;
  vec<uint32_t> vs;
  vec<uint32_t> vs_hist;
  vec<action> acs;
  vec<backpoint> save_hist;
  bool saved = false;
  
  uint32_t next_var = 0;
  uint32_t cl_len = 1;
  uint32_t cl_start = 0;
  uint32_t cl_off =0;
  tvec<goal> goals;
  tvec<trmo> path, lem;

  vec<trm*> init_cl = { lit_hash[0] };
  vec<trm*> cl = init_cl; 
  inline void deref(trmo &tm) const {
    while(tm.t->f >= 0 && subst[tm.t->f+tm.o].t) {
      tm = subst[tm.t->f+tm.o];
    }
  }
  
  bool lit_eq(const trmo &t1, const trm* t2t, uint32_t t2o);
  
  uint32_t lit_unify(const bool renamed, const trm* t1t, const uint32_t t1o, const trm* t2t, const uint32_t t2o);
  
  void restore_sub(uint32_t len);
  
  bool var_occurs_aux(uint32_t v, trmo t);
  
  bool var_occurs(uint32_t v, trmo t);
  
  void update_acs();
  
  bool act(action);
  
  bool inline act(uint32_t i){return act(acs[i]);}
  
  uint32_t save() {
    if(!saved){
      save_hist.emplace_back(backpoint{
        .cl = cl,
        .cl_start = cl_start,
        .cl_off = cl_off,
        .path = path,
        .lem = lem,
        .goals = goals,
        .subst_hist = subst_hist.size(),
        .next_var = next_var
      });
      saved = true;
    }
    return save_hist.size() - 1;
  }
  void restore(uint32_t bi) { restore(save_hist[bi]); save_hist.resize(bi+1); }
  void restart() {restore(0);}
 
  //function<void(uint32_t, uint32_t)> new_action;
  ostream& print_tm(ostream& os, const trm& t, uint32_t o) const;
  ostream& print_lit(ostream& os, const trm& l, uint32_t o) const;
  uint32_t lit_tm_size(bool is_lit, trmo t) const;
  uint32_t lit_tm_depth(bool is_lit, trmo t) const;
  void lit_tm_vars(bool is_lit, trmo t, vector<uint32_t>& vc, vector<uint32_t>& vi) const;
  prover& operator=(const prover&)=delete;
  prover(prover const&)=delete;

  enum {
    fea_global_num = 14,
    fea_action_num = 24
  };
  uint32_t fea_mod;
  vector<uint32_t> fea;
  vector<uint32_t> fea_indices;
  vector<uint32_t> vars_counts;
  vector<uint32_t> vars_indices;
  void fea_init(uint32_t _m) {
    fea_mod = _m;
    fea.resize(3*_m+fea_action_num);
  }
  void fea_global_update();
  void fea_action_update(uint32_t subst_before);
  inline void clear_vars();
  inline void clear_count_vars(uint32_t* count, uint32_t* unique);
  void clear_fea();
  ostream& print_fea(ostream&);
private:
  void restore(const backpoint& bp) {
    cl = bp.cl;
    cl_start = bp.cl_start;
    cl_off = bp.cl_off;
    path.restore(bp.path_len);
    lem.restore(bp.lem_len);
    goals.restore(bp.goals_len);
    restore_sub(bp.subst_hist_len);
    next_var = bp.next_var;
    saved = true;
  }
};

ostream& operator<<(ostream& os, const prover& p);

void problems_cleanup();

#endif
