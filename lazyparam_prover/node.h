#ifndef NODE_H_
#define NODE_H_

#include "google/protobuf/repeated_field.h"
#include "tptp.pb.h"

namespace tableau {

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

using NodeId = int32_t;

struct NodeIndex {
  NodeIndex(google::protobuf::RepeatedPtrField<tptp::Node> _nodes) {
    for(auto n : _nodes) {
      auto a = TYPE_ARITY.at(n.type());
      if(a!=CUSTOM_ARITY) n.set_arity(a);
      nodes[n.id()] = n;
    }
  }
  std::map<NodeId,tptp::Node> nodes;

  friend str show(const NodeIndex &idx) {
    vec<str> v;
    for(auto p : idx.nodes) {
      v.push_back(util::fmt("% -> %",p.first,p.second.DebugString()));
    }
    return util::fmt("{ % }",util::join(", ",v));;
  }
};


struct NodeInputStream {
  const tptp::Node &node_peek() { return idx.nodes.at(stream[i]); }
  const tptp::Node &node() { return idx.nodes.at(stream[i++]); }
  size_t arity() { return stream[i++]; }
  NodeInputStream(
    const NodeIndex &_idx,
    const google::protobuf::RepeatedField<NodeId> &_stream
  ) : idx(_idx), stream(_stream) {}

  friend str show(const NodeInputStream &s) {
    vec<str> v;
    for(size_t i=s.i; i<size_t(s.stream.size()); ++i) v.push_back(util::to_str(s.stream[i]));
    return util::join(" ",v);
  }
private:
  const NodeIndex &idx;
  const google::protobuf::RepeatedField<NodeId> &stream;
  size_t i=0;
};



struct RevNodeIndex {
private:
  std::map<tptp::Type,NodeId> standard_;
  std::map<unsigned,NodeId> funs_,preds_,vars_;
  std::set<NodeId> used;
  NodeId next_ = 0;
  NodeId next(){
    while(used.count(next_)) next_++;
    used.insert(next_);
    return next_;
  }
public:
  RevNodeIndex(){}
  INL RevNodeIndex(const RevNodeIndex &) = default;
  ~RevNodeIndex(){}
  RevNodeIndex(const google::protobuf::RepeatedPtrField<tptp::Node> &init) {
    for(auto n : init) {
      used.insert(n.id());
      *nodes.Add() = n;
      switch(n.type()) {
        case tptp::PRED: preds_[n.id()] = n.id(); break;
        case tptp::TERM_FUN: funs_[n.id()] = n.id(); break;
        case tptp::TERM_VAR: vars_[n.id()] = n.id(); break;
        default: standard_[n.type()] = n.id();
      }
    }
  }
public:
  google::protobuf::RepeatedPtrField<tptp::Node> nodes;

  NodeId var(unsigned i) { return vars_[i]; }
  NodeId fun(unsigned f) { return funs_[f]; }
  NodeId pref(unsigned p) { return preds_[p]; }
  NodeId standard(tptp::Type t){ return standard_[t]; }

  NodeId add_var(unsigned i) {
    if(vars_.count(i)) return vars_[i];
    NodeId id = next();
    vars_[i] = id;
    auto n = nodes.Add();
    n->set_type(tptp::TERM_VAR);
    n->set_id(id);
    return id;
  }

  NodeId add_fun(unsigned f, size_t arity, str name) {
    if(funs_.count(f)) return funs_[f];
    NodeId id = next();
    funs_[f] = id;
    auto n = nodes.Add();
    n->set_type(tptp::TERM_FUN);
    n->set_id(id);
    n->set_arity(arity);
    n->set_name(name);
    return id;
  }

  NodeId add_pred(unsigned p, size_t arity, str name) {
    if(preds_.count(p)) return preds_[p];
    NodeId id = next();
    preds_[p] = id;
    auto n = nodes.Add();
    n->set_type(tptp::PRED);
    n->set_id(id);
    n->set_arity(arity);
    n->set_name(name);
    return id;
  }

  NodeId add_standard(tptp::Type t) {
    DEBUG if(TYPE_ARITY.at(t)==CUSTOM_ARITY) error("% is not a standard node type",t);
    if(standard_.count(t)) return standard_[t];
    NodeId id = next();
    standard_[t] = id;
    auto n = nodes.Add();
    n->set_type(t);
    n->set_id(id);
    return id;
  }
};

struct NodeStream {
  vec<int32_t> stream;
  void add(NodeId v){ stream.push_back(int32_t(v)); }
};

} // namespace tableau

#endif  // NODE_H_
