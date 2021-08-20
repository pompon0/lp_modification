#ifndef TOOL_NODE_H_
#define TOOL_NODE_H_

#include "google/protobuf/repeated_field.h"
#include "tptp.pb.h"

namespace tool::node {

namespace {

enum {
  CUSTOM_ARITY = -2,
  VARIADIC_ARITY = -1,
};

const std::map<tptp::Type,int> TYPE_ARITY = {
  {tptp::PRED_EQ,2},
  {tptp::PRED,CUSTOM_ARITY},
  {tptp::TERM_FUN,CUSTOM_ARITY},
  {tptp::TERM_VAR,0},
  {tptp::FORM_NEG,1},
  {tptp::FORM_OR,VARIADIC_ARITY},
  {tptp::FORM_AND,VARIADIC_ARITY},
  {tptp::FORM_IFF,2},
  {tptp::FORM_IMPL,2},
  {tptp::FORM_RIMPL,2},
  {tptp::FORM_XOR,2},
  {tptp::FORM_NOR,2},
  {tptp::FORM_NAND,2},
  {tptp::FORM_TRUE,0},
  {tptp::FORM_FALSE,0},
  {tptp::FORALL,2},
  {tptp::EXISTS,2},
};

} // namespace

using Id = int32_t;

struct Index { 
private:
  std::map<tptp::Type,Id> standard_;
  std::map<unsigned,Id> funs_,preds_,vars_;
  std::map<Id,tptp::Node> nodes; 
  Id next_ = 0;
  Id add(tptp::Node n){
    while(nodes.count(next_)) next_++;
    n.set_id(next_);
    nodes.insert({next_,n});
    return next_;
  }
public:
  Index(){}
  Index(const Index&) = default;
  Index(google::protobuf::RepeatedPtrField<tptp::Node> _nodes) { FRAME("InputIndex");
    for(auto n : _nodes) {
      auto a = TYPE_ARITY.at(n.type());
      if(a!=CUSTOM_ARITY) n.set_arity(a);
      if(nodes.count(n.id())) error("duplicate node %",n.id());
      nodes.insert({n.id(),n});
      switch(n.type()) {
        case tptp::PRED: preds_[n.id()] = n.id(); break;
        case tptp::TERM_FUN: funs_[n.id()] = n.id(); break;
        case tptp::TERM_VAR: vars_[n.id()] = n.id(); break;
        default: standard_[n.type()] = n.id();
      }
    }
  } 
  google::protobuf::RepeatedPtrField<tptp::Node> get_nodes() {
    google::protobuf::RepeatedPtrField<tptp::Node> x;
    for(auto &[_,n] : nodes) *x.Add() = n;
    return x;
  }

  const tptp::Node &get(Id id) const { return nodes.at(id); }

  Id var(unsigned i) {
    if(vars_.count(i)) return vars_[i];
    tptp::Node n;
    n.set_type(tptp::TERM_VAR);
    return vars_[i] = add(n);
  }

  Id fun(unsigned f, size_t arity, str name) {
    if(funs_.count(f)) {
      DEBUG if(auto want = get(funs_[f]).arity(); want!=arity)
        error("arity = %, want %",arity,want);
      return funs_[f];
    }
    tptp::Node n;
    n.set_type(tptp::TERM_FUN);
    n.set_arity(arity);
    n.set_name(name);
    return funs_[f] = add(n);
  }

  Id pred(unsigned p, size_t arity, str name) {
    if(preds_.count(p)) {
      DEBUG if(auto want = get(preds_[p]).arity(); want!=arity)
        error("arity = %, want %",arity,want);
      return preds_[p];
    }
    tptp::Node n;
    n.set_type(tptp::PRED);
    n.set_arity(arity);
    n.set_name(name);
    return preds_[p] = add(n);
  }

  Id standard(tptp::Type t) {
    DEBUG if(TYPE_ARITY.at(t)==CUSTOM_ARITY) error("% is not a standard node type",t);
    if(standard_.count(t)) return standard_[t];
    tptp::Node n;
    n.set_type(t);
    return standard_[t] = add(n);
  }

  str pred_name(unsigned p) const {
    if(!preds_.count(p)) return "";
    return get(preds_.at(p)).name();
  }

  str fun_name(unsigned f) const {
    if(!funs_.count(f)) return "";
    return get(funs_.at(f)).name();
  }

  friend str show(const Index &idx) {
    vec<str> v;
    for(auto &[id,n] : idx.nodes) {
      v.push_back(util::fmt("% -> %",id,n.DebugString()));
    }
    return util::fmt("{ % }",util::join(", ",v));;
  }
};

struct InputStream {
  const tptp::Node &node_peek() { return idx.get(stream[i]); }
  const tptp::Node &node() { return idx.get(stream[i++]); }
  size_t arity() { return stream[i++]; }
  InputStream(
    const Index &_idx,
    const google::protobuf::RepeatedField<Id> &_stream
  ) : idx(_idx), stream(_stream) {}

  friend str show(const InputStream &s) {
    vec<str> v;
    for(size_t i=s.i; i<size_t(s.stream.size()); ++i) v.push_back(util::to_str(s.stream[i]));
    return util::join(" ",v);
  }
private:
  const Index &idx;
  const google::protobuf::RepeatedField<Id> &stream;
  size_t i=0;
};

struct OutputStream {
  OutputStream(Index &_idx) : idx(_idx) {}
  Index &idx;
  vec<int32_t> stream;
  void add(Id v){ stream.push_back(int32_t(v)); }
};

} // namespace tool::node

#endif  // TOOL_NODE_H_
