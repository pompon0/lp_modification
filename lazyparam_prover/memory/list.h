#ifndef MEMORY_LIST_H_
#define MEMORY_LIST_H_

#include "lazyparam_prover/log.h"
#include "lazyparam_prover/memory/alloc.h"
#include "lazyparam_prover/memory/maybe.h"

namespace tableau {

template<typename E> struct List {
private:
  struct Node { const Node *tail; const E head; };
  const Node *ptr;
  List(const Node *_ptr) : ptr(_ptr) {}
public:
  List() : List(0) {}
  List(Nothing) : List() {}
  List(E h, List<E> t) : List(alloc_init(Node{t.ptr,h})) {}
  explicit List(E h) : List(h,List()) {}
  
  bool empty() const { return !ptr; }
  const E head() const {
    DEBUG if(empty()) error("<0>.head()");
    return ptr->head;
  }
  const List<E> tail() const {
    DEBUG if(empty()) error("<0>.tail()");
    return List(ptr->tail);
  }

  size_t size() const { return empty() ? 0 : tail().size()+1; }

  friend List<E> operator+(E h, List<E> t) { return List(h,t); }
  List<E>& operator+=(E h){ return *this = List(h,*this); }

  template<typename T> List traverse(T t) const {
    if(empty()) return List();
    return t(head())+tail().traverse(t);
  }
};

// List adapter
template<typename Iso> struct ListA {
private:
  List<typename Iso::From> list;
public:
  explicit ListA(List<typename Iso::From> _list) : list(_list) {}
  bool empty() const { return list.empty(); }
  const typename Iso::To head() const { return Iso::to(list.head()); }
  const ListA tail() const { return ListA(list.tail()); }

  friend ListA operator+(typename Iso::To h, ListA t){ return ListA(Iso::From(h)+t.list); }
  ListA& operator+=(typename Iso::To h){ list += Iso::From(h); return *this; }

  vec<typename Iso::To> to_vec() const {
    vec<typename Iso::To> v;
    for(auto l = *this; !l.empty(); l = l.tail()) v.push_back(l.head());
    return v;
  }
};

}  // namespace tableau

#endif 
