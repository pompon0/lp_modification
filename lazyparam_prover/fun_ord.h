#ifndef FUN_ORD_H_
#define FUN_ORD_H_

#include <limits>
#include "solutions.pb.h"

// FunOrd represents an order on functor symbols.
struct FunOrd {
public:
  FunOrd(){}
  FunOrd(
    const solutions::FunOrd &proto,
    const google::protobuf::RepeatedPtrField<tptp::Node> &nodes
  ){
    std::map<str,Size> by_name;
    for(auto &f : proto.funs()){
      if(f.name()=="") error("name = \"\"");
      by_name[f.name()] = f.size();
    }
    for(auto &n : nodes) if(n.type()==tptp::TERM_FUN) {
      if(n.id()<0) error("n.id() = %",n.id());
      if(size_t(n.id())>=by_id.size()) by_id.resize(n.id()+1,def());
      if(auto it = by_name.find(n.name()); it!=by_name.end()) by_id[n.id()] = it->second;
    }
  }
 
  bool less(size_t f1, size_t f2) const {
    Size s1 = get(f1);
    Size s2 = get(f2);
    return s1!=s2 ? s1<s2 : f1<f2;
  }

private:
  using Size = int64_t;
  static constexpr Size def(){ return std::numeric_limits<Size>::max(); }
  vec<Size> by_id;
  Size get(size_t f) const { return f<by_id.size() ? by_id[f] : def(); }
};

#endif  // FUN_ORD_H_
