// - compute memory
// - use timer_create to stop particular provers
// - add "const &" everywhere where possible
// - add const to functions where possible

#include "ffprover.h"
#include <algorithm>
#include <caml/callback.h>

using namespace std;

#define fst(a) Field(a, 0)
#define snd(a) Field(a, 1)
#define third(a) Field(a, 2)
#define fourth(a) Field(a, 3)

/*static inline uint32_t caml_length(value l) {
  uint32_t ret = 0;
  for (; l != Val_emptylist; l = snd(l)) ++ret;
  return ret;
}*/

static unordered_map<int64_t, string> func_to_string;
static vector<trm*> trm_hash;
static vector<trm*> lit_hash;

ostream& trm::print_raw_tm(ostream& os) const {
  if (f >= 0){ os << 'V' << f; return os; }
  os << func_to_string[-f];
  if (n == 0) return os;
  os << "(";
  a[0]->print_raw_tm(os);
  for (uint32_t i = 1; i < n; ++i) {
    os << ",";
    a[i]->print_raw_tm(os);
  }
  os << ")";
  return os;
}

ostream& trm::print_raw_lit(ostream& os) const {
  if (func_to_string[abs(f)] == "=") {
    a[0]->print_raw_tm(os); os << (f<0?"~=":"="); a[1]->print_raw_tm(os); return os;
  }
  if (f < 0) os << "~";
  os << func_to_string[abs(f)];
  if (n == 0) return os;
  os << "(";
  a[0]->print_raw_tm(os);
  for (uint32_t i = 1; i < n; ++i) {
    os << ",";
    a[i]->print_raw_tm(os);
  }
  os << ")";
  return os;
}

ostream& operator<<(ostream& os, const contra& c){
  os << hex << (unsigned)c.cno % 4095 << dec;
  return os;
}

problem::problem(bool conj, int def, bool contentnames, int reo, const char* fname) {
  init(caml_file_mat(conj,def,contentnames,reo,fname));
}

ostream& operator<<(ostream& os, const problem& p){
  for (const contra& x : p.contras) {
    os << "C" << x << ' ';
    x.lit1->print_raw_lit(os);
    for (uint32_t i = 0; i < x.rest_len; ++i) {
      os << " | ";
      x.rest[i]->print_raw_lit(os);
    }
    os << endl;
  }
  return os;
}

ostream& operator<<(ostream& os, const action& a){
  switch (a.type) {
  case ac_ext: os << "E" << a.no; break;
  case ac_red: os << "R" << a.no; break;
  case ac_lem: os << "L" << a.no; break;
  }
  return os;
}

ostream& prover::print_tm(ostream& os, const trm& t, uint32_t o) const {
  if (t.f >= 0) {
    if (unsigned(t.f) + o < subst.size() && subst[t.f + o].t)
      return print_tm(os, *subst[t.f + o].t, subst[t.f+o].o);
    else
      { os << 'V' << t.f+o; return os; }
  }
  os << func_to_string[-t.f];
  if (t.n == 0) return os;
  os << "(";
  print_tm(os, *t.a[0], o);
  for (uint32_t i = 1; i < t.n; ++i) {
    os << ",";
    print_tm(os, *t.a[i], o);
  }
  os << ")";
  return os;
}

ostream& prover::print_lit(ostream& os, const trm& l, uint32_t o) const {
  if (func_to_string[abs(l.f)] == "=") {
    print_tm(os, *l.a[0], o); os << (l.f<0?"~=":"="); print_tm(os, *l.a[1], o); return os;
  }
  if (l.f < 0) os << "~";
  os << func_to_string[abs(l.f)];
  if (l.n == 0) return os;
  os << "(";
  print_tm(os, *l.a[0], o);
  for (uint32_t i = 1; i < l.n; ++i) {
    os << ",";
    print_tm(os, *l.a[i], o);
  }
  os << ")";
  return os;
}

void prover::update_acs(){
  assert(cl_start < cl_len);
  acs.clear();
  for (uint32_t i = cl_start; i < cl_len; ++i)
    for (uint32_t j = path.begin(); j != path.end(); j = path.next(j))
      if (lit_eq(path[j], cl[i], cl_off)) return;
  for (uint32_t i = lem.begin(); i != lem.end(); i = lem.next(i)) {
    const int32_t unif = lit_unify(false, lem[i].t, lem[i].o, cl[cl_start], cl_off);
    if (unif >= 0) {
      if (subst_hist.size() == (unsigned)unif) acs.clear(); // Safe action, no assignments
      acs.emplace_back(ac_lem, i);
      //if (new_action) new_action(unif, 0);
      if (subst_hist.size() == (unsigned)unif) return;
      restore_sub(unif);
    }
  }
  for (uint32_t i = path.begin(); i != path.end(); i = path.next(i)){
    const int32_t unif = lit_unify(false, path[i].t, path[i].o, cl[cl_start], cl_off);
    if (unif >= 0) {
      if (subst_hist.size() == (unsigned)unif) acs.clear(); // Safe action, no assignments
      acs.emplace_back(ac_red, i);
      //if (new_action) new_action(unif, 0);
      if (subst_hist.size() == (unsigned)unif) return;
      restore_sub(unif);
    }
  }
  const auto cs_range = prob.lit_contr.equal_range(cl[cl_start]->f);
  for (auto c = cs_range.first; c != cs_range.second; ++c) {
    if (subst.size() < next_var + prob.contras[c->second].vars) {
      vs.resize(next_var + prob.contras[c->second].vars);
      subst.resize(next_var + prob.contras[c->second].vars);
    }
    const int32_t unif = lit_unify(true, cl[cl_start], cl_off, prob.contras[c->second].lit1, next_var);
    if (unif >= 0) {
      bool safe = prob.contras[c->second].rest_len == 0;
      if (safe)
        for (uint32_t i = unif; i < subst_hist.size(); ++i)
          if (subst_hist[i] < next_var) safe = false;
      if (safe) acs.clear();
      acs.emplace_back(ac_ext, c->second);
      //if (new_action) new_action(unif, prob.contras[c->second].vars);
      if (safe) return;
      restore_sub(unif);
    }
  }
}

// Assumes vars in vs have been checked
bool prover::var_occurs_aux(uint32_t v, const trmo t){
  if (t.t->f >= 0)
    if (vs[t.t->f+t.o]) return false;
    else if (t.t->f+t.o == v) return true;
    else if (!subst[t.t->f+t.o].t) return false;
    else {
      const bool ret = var_occurs_aux(v, subst[t.t->f+t.o]);
      if (not ret) {
        vs[t.t->f+t.o] = true;
        vs_hist.push_back(t.t->f+t.o);
      }
      return ret;
    }
  else {
    trmo t2 = t;
    for (uint32_t j = 0; j < t.t->n; ++j) {
      t2.t = t.t->a[j];
      if (var_occurs_aux(v, t2)) return true;
    }
    return false;
  }
}

bool prover::var_occurs(uint32_t v, trmo t){
  while (!vs_hist.empty()) {
    vs[vs_hist.back()] = false;
    vs_hist.pop_back();
  }
  return var_occurs_aux(v, t);
}

bool prover::lit_eq(const trmo &t1, const trm* t2t, uint32_t t2o){
  //cout << "Eq: "; print_lit(cout, *t1.t, t1.o); cout << " : "; print_lit(cout, *t2t, t2o); cout << endl;
  if (t1.t->f != t2t->f) return false;
  ts.clear();
  for (uint32_t tno = 0; tno < t2t->n; ++tno)
    ts.emplace_back(t1.t->a[tno], t1.o, t2t->a[tno], t2o); // ts are not related to clauses
  while (not ts.empty()) {
    auto t = ts.back(); ts.pop_back(); // t is not related to clauses
    deref(t.t1); deref(t.t2);
    if ((t.t1.t->f >= 0) && (t.t2.t->f >= 0))
      if (t.t1.t->f+t.t1.o != t.t2.t->f+t.t2.o) return false;
      else continue;
    else if (t.t1.t->f != t.t2.t->f) return false;
    else if (t.t1.t->f < 0)
      for (uint32_t i = 0; i < t.t1.t->n; ++i)
        ts.emplace_back(t.t1.t->a[i], t.t1.o, t.t2.t->a[i], t.t2.o);
  }
  return true;
}

inline void prover::restore_sub(uint32_t len){
  while (subst_hist.size() > len) {
    subst[subst_hist.back()].t = NULL;
    subst_hist.pop_back();
  }
}

// Returns -1 if failed or substitution restore number
// If renamed is true, we can skip occur check
uint32_t prover::lit_unify(bool renamed, const trm* t1t, const uint32_t t1o, const trm* t2t, const uint32_t t2o){
  //cout << " Unify" << (renamed?'T':'F') << ": "; print_lit(cout, *t1t, t1o); cout << " : "; print_lit(cout, *t2t, t2o); cout << endl;
  if (t1t->f != -t2t->f) return -1;
  ts.clear();
  for (uint32_t tno = 0; tno < t2t->n; ++tno)
    ts.emplace_back(t1t->a[tno], t1o, t2t->a[tno], t2o);
  const uint32_t init_subst_hist = subst_hist.size();
  while (not ts.empty()) {
    auto t = ts.back(); ts.pop_back();
    deref(t.t1); deref(t.t2);
    //cout << "  Unify1: "; print_tm(cout, *t.t1.t, t.t1.o); cout << " : "; print_tm(cout, *t.t2.t, t.t2.o); cout << endl;
    if (t.t1.t == t.t2.t && t.t1.o == t.t2.o) continue; // TODO: see if useful
    if (t.t1.t->f >= 0 && t.t2.t->f >= 0) {
      if (t.t1.t->f + t.t1.o == t.t2.t->f + t.t2.o) continue;
      subst[t.t2.t->f + t.t2.o] = t.t1;
      subst_hist.emplace_back(t.t2.t->f + t.t2.o);
      renamed = false;
    } else if (t.t1.t->f < 0 && t.t2.t->f < 0) {
      if (t.t1.t->f != t.t2.t->f) {
        restore_sub (init_subst_hist); return -1;
      }
      for (uint32_t i = 0; i < t.t1.t->n; ++i) {
        ts.emplace_back(t.t1.t->a[i], t.t1.o, t.t2.t->a[i], t.t2.o);
      }
    } else {
      if (t.t2.t->f >= 0) swap(t.t1, t.t2);
      if (renamed || not var_occurs(t.t1.t->f+t.t1.o, t.t2)) {
        subst[t.t1.t->f+t.t1.o] = t.t2;
        subst_hist.emplace_back(t.t1.t->f+t.t1.o);
        renamed = false;
      } else {
        restore_sub (init_subst_hist); return -1;
      }
    }
  }
  return init_subst_hist;
}

bool prover::act(action a){
  switch (a.type) {
  case ac_ext:
    lit_unify(true, cl[cl_start], cl_off, prob.contras[a.no].lit1, next_var);
    //    goals.push(cl, cl_len, cl_start + 1, cl_off, path.current(), lem.current(), cl_off, cl[cl_start]);
    goals.push(cl, cl_len, cl_start + 1, cl_off, path.current(), lem.current(), next_var, prob.contras[a.no].lit1);
    path.push(cl[cl_start], cl_off);
    cl = prob.contras[a.no].rest;
    cl_start = 0;
    cl_len = prob.contras[a.no].rest_len;
    cl_off = next_var;
    next_var += prob.contras[a.no].vars;
    break;
  case ac_red:
    lit_unify(false, path[a.no].t, path[a.no].o, cl[cl_start], cl_off);
    cl_start++;
    break;
  case ac_lem:
    lit_unify(false, lem[a.no].t, lem[a.no].o, cl[cl_start], cl_off);
    cl_start++;
  }
  while (!goals.empty() && cl_start == cl_len) {
    const goal& gl = goals.pop();
    cl = gl.cl; cl_len = gl.cl_len; cl_start = gl.cl_start; cl_off = gl.cl_off;
    path.subset(gl.path_len); lem.subset(gl.lem_len);
    lem.push(gl.nlem, gl.nlem_off);
  }
  saved = false;
  return (cl_start == cl_len);
}

uint32_t prover::lit_tm_size(bool is_lit, trmo t) const {
  if (!is_lit) deref(t);
  uint32_t ret = 1;
  for (uint32_t i = 0; i < t.t->n; ++i) {
    trmo tt(t.t->a[i],t.o);
    ret += lit_tm_size(false, tt);
  }
  return ret;
}

uint32_t prover::lit_tm_depth(bool is_lit, trmo t) const {
  if (!is_lit) deref(t);
  uint32_t ret = 0;
  for (uint32_t i = 0; i < t.t->n; ++i) {
    trmo tt(t.t->a[i],t.o);
    ret = max(lit_tm_depth(false, tt), ret);
  }
  return 1 + ret;
}

void prover::lit_tm_vars(bool is_lit, trmo t, vector<uint32_t>& vc, vector<uint32_t>& vi) const {
  if (!is_lit) deref(t);
  if (!is_lit && t.t->f >= 0) {
    const uint32_t v = t.t->f + t.o;
    if (vc[v]++ == 0) vi.emplace_back(v);
  } else for (uint32_t i = 0; i < t.t->n; ++i) {
    trmo tt(t.t->a[i],t.o);
    lit_tm_vars(false, tt, vc, vi);
  }
}

ostream& operator<<(ostream& os, const prover& p){
  os << "Cl:";
  for (uint32_t i = p.cl_start; i < p.cl_len; ++i){
    os << ' '; p.print_lit(os, *p.cl[i], p.cl_off);
  }
  os << " Lm:";
  for (uint32_t i = p.lem.begin(); i != p.lem.end(); i = p.lem.next(i)){
    os << ' '; p.print_lit(os, *p.lem[i].t, p.lem[i].o);
  }
  os << " Ac: ";
  for (uint32_t i = 0; i < p.acs.size(); ++i){
    os << ' ' << p.acs[i];
  }
  os << " NextV: " << p.next_var << endl;
  return os;
}

void problems_cleanup(){
  for (auto v : trm_hash) delete v;
  for (auto v : lit_hash) delete v;
}

inline void prover::clear_vars(){
  while (!vars_indices.empty()) {
    vars_counts[vars_indices.back()] = 0;
    vars_indices.pop_back();
  }
}

inline void prover::clear_count_vars(uint32_t* count, uint32_t* unique) {
  while (!vars_indices.empty()) {
    *count+=vars_counts[vars_indices.back()];
    (*unique)++;
    vars_counts[vars_indices.back()] = 0;
    vars_indices.pop_back();
  }
}

inline void prover::clear_fea(){
  while (!fea_indices.empty()) {
    fea[fea_indices.back()] = 0;
    fea_indices.pop_back();
  }
}

void prover::fea_global_update() {
  // clear_vars(); Always cleared...
  vars_counts.resize(next_var);
  clear_fea();
  // Path no, size, vars
  uint32_t path_size = 0;
  for (uint32_t i = 0; i < path.size(); ++i) {
    path_size += lit_tm_size(true, path[i]);
    lit_tm_vars(true, path[i], vars_counts, vars_indices);
  }
  uint32_t path_vars = 0, path_unique_vars = 0;
  clear_count_vars(&path_vars, &path_unique_vars);
  fea[0] = path.size();
  fea[1] = path_size;
  fea[2] = path_vars;
  fea[3] = path_unique_vars;
  // Subst no, size
  fea[4] = subst_hist.size();
  uint32_t subst_size = 0;
  for (uint32_t i = 0; i < subst_hist.size(); ++i)
    subst_size += lit_tm_size(false, subst[subst_hist[i]]);
  fea[5] = subst_size;
  // Goals no, size, max, max_depth, vars
  uint32_t goals_no = cl_len - cl_start;
  uint32_t goals_sum_size = 0;
  uint32_t goals_max_size = 0;
  uint32_t goals_max_depth = 0;
  for (uint32_t i = cl_start; i < cl_len; ++i) {
    const uint32_t s = lit_tm_size(true, trmo(cl[i], cl_off));
    const uint32_t d = lit_tm_depth(true, trmo(cl[i], cl_off));
    lit_tm_vars(true, trmo(cl[i], cl_off), vars_counts, vars_indices);
    goals_sum_size += s;
    goals_max_size = max(goals_max_size, s);
    goals_max_depth = max(goals_max_depth, d);
  }
  for (uint32_t j = 0; j < goals.size(); ++j) {
    goals_no += goals[j].cl_len - goals[j].cl_start;
    for (uint32_t i = goals[j].cl_start; i < goals[j].cl_len; ++i) {
      const uint32_t s = lit_tm_size(true, trmo(goals[j].cl[i], goals[j].cl_off));
      const uint32_t d = lit_tm_depth(true, trmo(goals[j].cl[i], goals[j].cl_off));
      lit_tm_vars(true, trmo(goals[j].cl[i], goals[j].cl_off), vars_counts, vars_indices);
      goals_sum_size += s;
      goals_max_size = max(goals_max_size, s);
      goals_max_depth = max(goals_max_depth, d);
    }
  }
  uint32_t goals_vars = 0, goals_unique_vars = 0;
  clear_count_vars(&goals_vars, &goals_unique_vars);
  fea[6] = goals_no;
  fea[7] = goals_sum_size;
  fea[8] = goals_max_size;
  fea[9] = goals_max_depth;
  fea[10] = goals_vars;
  fea[11] = goals_unique_vars;
  // Lem length
  fea[12] = lem.size();
  // Vars
  fea[13] = next_var;
}

void prover::fea_action_update(uint32_t subst_before) {
  vars_counts.resize(next_var);
  // News subst assignments: number, size, old-part
  fea[14] = subst_hist.size() - subst_before;
  uint32_t subst_add_size = 0;
  uint32_t subst_old_vars = 0;
  for (uint32_t i = subst_before; i < subst_hist.size(); ++i) {
    if (subst_hist[i] >= subst_before) subst_old_vars++;
    subst_add_size += lit_tm_size(false, subst[subst_hist[i]]);
  }
  fea[15] = subst_add_size;
  fea[16] = subst_old_vars;
  // New goals no, size, max, max_depth, vars
  uint32_t goals_no = cl_len - cl_start;
  uint32_t goals_sum_size = 0;
  uint32_t goals_max_size = 0;
  uint32_t goals_max_depth = 0;
  for (uint32_t i = cl_start; i < cl_len; ++i) {
    const uint32_t s = lit_tm_size(true, trmo(cl[i], cl_off));
    const uint32_t d = lit_tm_depth(true, trmo(cl[i], cl_off));
    lit_tm_vars(true, trmo(cl[i], cl_off), vars_counts, vars_indices);
    goals_sum_size += s;
    goals_max_size = max(goals_max_size, s);
    goals_max_depth = max(goals_max_depth, d);
  }
  uint32_t goals_vars = 0, goals_unique_vars = 0;
  clear_count_vars(&goals_vars, &goals_unique_vars);
  //clear_vars();
  fea[17] = goals_no;
  fea[18] = goals_sum_size;
  fea[19] = goals_max_size;
  fea[20] = goals_max_depth;
  fea[21] = goals_vars;
  fea[22] = goals_unique_vars;
  // TODO: STRONG ASSUMPTION
  fea[23] = next_var - fea[13];
}

ostream& prover::print_fea(ostream& os) {
  sort(fea_indices.begin(), fea_indices.end());
  for(uint32_t i = 0; i < fea_indices.size(); ++i)
    os << " " << fea_indices[i] << ':' << fea[fea_indices[i]];
  return os;
}
