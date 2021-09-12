#ifndef FEATURES_H_
#define FEATURES_H_

#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/term.h"
#include "tool/node.h"

namespace features {

namespace {
namespace encoding {

struct Hash {
  Hash() : val(0), mul(1) {}
  Hash(uint64_t _val) : val(_val), mul(c) {}
  Hash& push(Hash h){ val += h.val*mul; mul *= h.mul; return *this; }
  uint64_t sum() const { return val; }
private:
  // SBDM hashing function.
  enum : uint64_t { c = (1<<6)+(1<<16)-1 };
  uint64_t val,mul;
};

// symbols for encoding features as strings
enum : uint64_t {
  // DO NOT change the numbering,
  // otherwise you will have to retrain from scratch.
  var_begin = 1,
  fun_begin = 2,
  pred_begin = 3,
  eq_begin = 5,
  sign_begin = 4,
  end = 5,

  horizontal_begin = 6,
  vertical_begin = 7,
  
  goal = 8, // open branches
  lemma = 17, // literals assumed to be true (available for exact matches)
  path = 9, // current branch to solve
  proof = 13, // current partial proof
  free_vars = 15, // free variables in the proof
  total_vars = 16, // (implementation specific) total vars used in the proof
  tasks = 14, // (implementation specific) tasks to complete

  length = 10,
  occurences = 11,
  max_depth = 12,
};

template<size_t n> struct Window {
  INL Window& push(Hash v){ data[next++%n] = v; return *this; }
  INL Hash hash() const {
    Hash h;
    for(size_t i=0; i<n; i++) h.push(data[(next+i)%n]);
    return h;
  }
  INL size_t length() const { return next; }
private:
  size_t next = 0;
  Hash data[n] = {};
};

// If we decide items order before MCTS, it has to be based on the problem features.
//
// We can however have a separate model to evaluate each new branch separately
// and sort the items according to that evaluation. But that would have to be
// trained on resulting search space size.

INL Hash term_head(const tool::node::Index &idx, tableau::Term t) { FRAME("term_head");
  switch(t.type()) {
  case tableau::Term::VAR:
    return Hash(var_begin).push(end);
  case tableau::Term::FUN: {
    Hash h(fun_begin);
    // Skolem (and other artificial functors) are expected to have an empty name.
    for(auto x : idx.fun_name(tableau::Fun(t).fun())) h.push(x);
    return h.push(end);
  }
  default:
    error("t.type() = %",t.type());
  }
}

INL Hash pred_head(const tool::node::Index &idx, tableau::Atom a) { FRAME("pred_head");
  //TODO: probably also distinguish the special symbols introduced by LPMOD etc.
  if(a.pred()==tableau::Atom::EQ) return Hash(eq_begin).push(end);
  Hash h(pred_begin);
  //TODO: cache it somewhere, recomputation takes way too much time
  for(auto x : idx.pred_name(a.pred())) h.push(x);
  return h.push(end);
}

INL Hash sign(bool sign) {
  return Hash(sign_begin).push(sign).push(end);
}

INL uint64_t vertical_feature(Window<3> w) {
  return Hash(vertical_begin).push(w.hash()).push(end).sum();
}

INL uint64_t horizontal_feature(Hash h) {
  return Hash(horizontal_begin).push(h).push(end).sum();
}

}  // namespace encoding
}  // namespace

struct Space {
  INL Space(memory::Alloc &A, size_t size) : v(A,size) {
    if(size<1) error("size = %, want >=1",size);
    for(size_t i=0; i<size; i++) v[i] = 0;
  }
  memory::Array<size_t> v;
};

struct SubSpace {
  INL SubSpace(Space *s) : space(s) {}
  INL SubSpace sub(encoding::Hash x){ return SubSpace(space,encoding::Hash(pref).push(x)); }
  INL size_t & at(encoding::Hash x){
    return space->v[encoding::Hash(pref).push(x).sum()%space->v.size()];
  }
private:
  INL SubSpace(Space *s, encoding::Hash p) : space(s), pref(p) {}
  Space *space;
  encoding::Hash pref;
};

// Returns the head symbol. Accumulates all the vertical features.
static encoding::Hash add(SubSpace s, const tool::node::Index &idx, const tableau::Val &val, encoding::Window<3> path, tableau::Term t) { FRAME("ActionVec::add(%)",show(t));
  auto ts = encoding::term_head(idx,val.shallow_eval(t));
  path.push(ts);
  s.sub(encoding::occurences).at(ts) += 1;
  util::maxi(s.sub(encoding::max_depth).at(ts),path.length());
  s.sub(encoding::vertical_begin).at(path.hash()) += 1;
  switch(t.type()) {
    case tableau::Term::VAR: return ts;
    case tableau::Term::FUN: {
      tableau::Fun f(t);
      encoding::Hash h(ts);
      for(size_t i=0; i<f.arg_count(); i++) h.push(add(s,idx,val,path,f.arg(i)));
      s.sub(encoding::horizontal_begin).at(h) += 1;
      return ts;
    }
    default:
      error("t.type() = %",t.type());
  }
}

static void add(SubSpace s, const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a) { FRAME("ActionVec::add(%)",show(a));
  auto ss = encoding::sign(a.sign()); 
  auto ps = encoding::pred_head(idx,a);
  s.sub(encoding::sign_begin).at(a.sign()) += 1;
  s.sub(encoding::occurences).at(ps) += 1;
  encoding::Window<3> w; w.push(ss).push(ps);
  encoding::Hash h(ps);
  for(size_t i=0; i<a.arg_count(); i++) h.push(add(s,idx,val,w,a.arg(i)));
  s.sub(encoding::horizontal_begin).at(h) += 1;
}

// TODO: this definitely deserves more structure
struct ActionVec {
  explicit ActionVec(memory::Alloc &A, size_t space_size) : features(A,space_size) {}
  Space features;
  size_t new_goal_count = 0;
  size_t lemma_count = 0;
  size_t path_length = 0;

  void add_new_goal(const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a){
    new_goal_count++;
    auto sub = SubSpace(&features).sub(encoding::goal);
    sub.at(encoding::length)++;
    add(sub,idx,val,a);
  }
  void add_lemma(const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a) {
    lemma_count++;
    auto sub = SubSpace(&features).sub(encoding::lemma);
    sub.at(encoding::length)++;
    add(sub,idx,val,a);
  }
  void add_path(const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a){
    path_length++;
    auto sub = SubSpace(&features).sub(encoding::path);
    sub.at(encoding::length)++;
    add(sub,idx,val,a);
  }
};

struct StateVec {
  explicit StateVec(memory::Alloc &A, size_t space_size) : features(A,space_size) {}
  Space features;
  size_t goal_count = 0;
  size_t proof_size = 0;

  // proof size in clauses (aka nodes, aka cost)
  void set_proof_size(size_t x){
    proof_size = x;
    SubSpace(&features).sub(encoding::proof).at(encoding::length) = x;
  }
  // number of free variables
  void set_free_vars_count(size_t x){ SubSpace(&features).sub(encoding::free_vars).at(encoding::length) = x; }
  // number of variables total
  void set_total_vars_count(size_t x){ SubSpace(&features).sub(encoding::total_vars).at(encoding::length) = x; }

  // number of tasks
  void set_task_count(size_t x){ SubSpace(&features).sub(encoding::tasks).at(encoding::length) = x; }

  void add_goal(const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a){
    goal_count++;
    auto sub = SubSpace(&features).sub(encoding::goal);
    sub.at(encoding::length)++;
    add(sub,idx,val,a);
  }

  // proof size in literals
  // depth of the open branches
  // total proof depth
  // number of free variables (total/in current branch)
  // number of free variables present only in a single branch
  // number of literals per predicate? (can we do that?)
};

} // namespace features

#endif  // FEATURES_H_
