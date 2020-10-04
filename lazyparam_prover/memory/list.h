#ifndef MEMORY_LIST_H_
#define MEMORY_LIST_H_

#include "utils/log.h"
#include "lazyparam_prover/memory/maybe.h"
#include "lazyparam_prover/memory/stack.h"

namespace memory {

template<typename E> struct List {
private:
  struct Node { const Node *tail; const E head; };
  const Node *ptr;
  INL List(const Node *_ptr) : ptr(_ptr) {}
public:
  INL List() : List((const Node*)0) {}
  INL List(Nothing) : List() {}
  INL List(memory::Alloc &A, E h, List<E> t) : List(A.alloc_init(Node{t.ptr,h})) {}
  INL List(memory::Alloc &A, E h) : List(A,h,List()) {}
  
  INL bool empty() const { return !ptr; }
  INL const E head() const {
    DEBUG if(empty()) error("<0>.head()");
    return ptr->head;
  }
  INL const List<E> tail() const {
    DEBUG if(empty()) error("<0>.tail()");
    return List(ptr->tail);
  }

  INL void push(memory::Alloc &a, E h){ *this = List(a,h,*this); }
  INL [[nodiscard]] List add(memory::Alloc &a, E h) const { return List(a,h,*this); }
  size_t size() const { return empty() ? 0 : tail().size()+1; }
  
  //TODO: consider some kind of hash consing instead.
  INL bool pointer_equal(List l){ return ptr==l.ptr; }
};

}  // namespace memory

#endif  // MEMORY_LIST_H_ 
