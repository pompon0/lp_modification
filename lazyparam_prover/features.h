#ifndef FEATURES_H_
#define FEATURES_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/term.h"

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
  var_begin = 1,
  fun_begin = 2,
  pred_begin = 3,
  eq_begin = 5,
  sign_begin = 4,
  end = 5,

  horizontal_begin = 6,
  vertical_begin = 7,
  goal_atom = 8,
  path_atom = 9,
};

template<size_t n> struct Window {
  Window& push(Hash v){ data[next++%n] = v; return *this; }
  Hash hash() const {
    Hash h;
    for(size_t i=0; i<n; i++) h.push(data[(next+i)%n]);
    return h;
  }
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
  //TODO probably also distinguish the special symbols introduced by LPMOD etc.
  if(a.pred()==tableau::Atom::EQ) return Hash(eq_begin).push(end);
  Hash h(pred_begin);
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
  INL Space(size_t size) : v(size,0) { if(size<1) error("size = %, want >=1",size); }
  vec<size_t> v;
};

struct SubSpace {
  INL SubSpace(Space *s) : space(s) {}
  INL SubSpace sub(encoding::Hash x){ return SubSpace(space,encoding::Hash(pref).push(x)); }
  INL void add(encoding::Hash x, size_t val){
    space->v[encoding::Hash(pref).push(x).sum()%space->v.size()] += val;
  }
private:
  INL SubSpace(Space *s, encoding::Hash p) : space(s), pref(p) {}
  Space *space;
  encoding::Hash pref;
};

// shouldn't this be rather a state diff? IMO it should summarize all new branches
struct ActionVec {
  explicit ActionVec(size_t space_size) : features(space_size) {}
  
  Space features;

  void add_goal(const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a){ add(SubSpace(&features).sub(encoding::goal_atom),idx,val,a); }
  void add_path(const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a){ add(SubSpace(&features).sub(encoding::path_atom),idx,val,a); }

private:
  static void add(SubSpace s, const tool::node::Index &idx, const tableau::Val &val, tableau::Atom a) { FRAME("ActionVec::add(%)",show(a));
    s.sub(encoding::sign_begin).add(a.sign(),1);
    auto ss = encoding::sign(a.sign()); 
    auto ps = encoding::pred_head(idx,a);
    encoding::Window<3> w; w.push(ss).push(ps);
    encoding::Hash h(ps);
    for(size_t i=0; i<a.arg_count(); i++) h.push(add(s,idx,val,w,a.arg(i)));
    s.sub(encoding::horizontal_begin).add(h,1);
  }

  // Returns the head symbol. Accumulates all the vertical features.
  static encoding::Hash add(SubSpace s, const tool::node::Index &idx, const tableau::Val &val, encoding::Window<3> path, tableau::Term t) { FRAME("ActionVec::add(%)",show(t));
    auto ts = encoding::term_head(idx,val.shallow_eval(t));
    path.push(ts);
    s.sub(encoding::vertical_begin).add(path.hash(),1);
    switch(t.type()) {
      case tableau::Term::VAR: return ts;
      case tableau::Term::FUN: {
        tableau::Fun f(t);
        encoding::Hash h(ts);
        for(size_t i=0; i<f.arg_count(); i++) h.push(add(s,idx,val,path,f.arg(i)));
        s.sub(encoding::horizontal_begin).add(h,1);
        return ts;
      }
      default:
        error("t.type() = %",t.type());
    }
  }
};



struct StateVec {
  size_t proof_size; // in clauses
  size_t open_branches;
  // proof size in literals
  // depth of the current branch
  // depth of the open branches
  // total proof depth
  // number of variables 
  // number of free variables (total/in current branch)
  // number of free variables present only in a single branch
  // number of literals per predicate? (can we do that?)
};

} // namespace features

#endif  // FEATURES_H_
