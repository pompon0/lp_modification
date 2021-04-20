#ifndef FEATURES_H_
#define FEATURES_H_

#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/term.h"

namespace features {

struct Hash {
  Hash() : val(0), mul(1) {}
  Hash(uint64_t _val) : val(_val), mul(c) {}
  Hash& push(Hash h){ val += h.val*mul; mul *= h.mul; return *this; }
  uint64_t sum() const { return val; }
private:
  // SBDM hashing function.
  enum : uint64_t { c = (1<<6)+(1<<16)-1 };
  uint64_t mul;
  uint64_t val;
};

namespace symbols {
  enum : uint64_t {
    var = 1,
    fun = 2,
    pred = 3,
    eq = 5,
    sign = 4,

    separator = 5,
    horizontal = 6,
    vertical = 7,
  };
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

static INL Hash term_symbol(const tool::node::Index &idx, tableau::Term t) {
  switch(t.type()) {
  case tableau::Term::VAR:
    return Hash(symbol::var).push(separator);
  case tableau::Term::FUN:
    Hash h(symbol::fun);
    // Skolem (and other artificial functors) are expected to have an empty name.
    for(auto x : idx.fun_name(Fun(t).fun())) h.push(x);
    return h.push(separator);
  default:
    error("t.type() = %",t.type());
  }
}

static INL Hash pred_symbol(const tool::node::Index &idx, tableau::Atom a) {
  //TODO probably also distinguish the special symbols introduced by LPMOD etc.
  if(a.pred()==Atom::EQ) return Hash(symbol::eq).push(separator);
  Hash h(symbol::pred);
  for(auto x : idx.pred_name(a.pred())) h.push(x);
  return h.push(separator);
}

static INL Hash sign_symbol(bool sign) {
  return Hash(symbol::sign).push(sign).push(separator);
}

static INL uint64_t vertical_feature(Window<3> w) {
  return Hash(symbol::vertical).push(w.hash()).sum();
}

static INL uint64_t horizontal_feature(Hash h) {
  return Hash(symbol::horizontal).push(h).sum();
}

// shouldn't this be rather a state diff? IMO it should summarize all new branches
struct ActionVec {
  bool mcts_node = true;
  size_t positive_atoms = 0;
  size_t negative_atoms = 0;

  vec<size_t> hashed;

  void add(const tool::node::Index &idx, tableau::Atom a) {
    if(a.sign()) positive_atoms++; else negative_atoms++;
    auto ss = sign_symbol(a.sign()); 
    auto ps = pred_symbol(idx,a);
    Window<3> w;
    w.push(ss).push(ps);
    Hash h(ps);
    for(size_t i=0; i<a.arg_count(); i++) h.push(add(idx,w,a.arg(i)));
    hashed[horizontal_feature(h)%hashed.size()]++;
  }

  // Returns the root symbol. Accumulates all the vertical features.
  Hash add(const tool::node::Index &idx, Window<3> path, tableau::Term t) {
    auto ts = term_symbol(idx,t);
    path.push(ts);
    hashed[vertical_feature(path)%hashed.size()]++;
    Hash h(ts);
    switch(t.type()) {
      case tableau::Term::VAR: return ts;
      case tableau::Term::FUN:
        for(size_t i=0; i<Fun(t).arg_count(); i++) h.push(add(idx,path,Fun(t).arg(i)));
        hashed[horizontal_feature(h)%hashed.size()]++;
        return ts;
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
