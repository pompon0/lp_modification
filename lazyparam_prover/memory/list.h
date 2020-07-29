#ifndef MEMORY_LIST_H_
#define MEMORY_LIST_H_

#include "lazyparam_prover/log.h"
#include "lazyparam_prover/memory/maybe.h"
#include "lazyparam_prover/memory/stack.h"

namespace tableau {

template<typename E> struct List {
private:
  struct Node { const Node *tail; const E head; };
  const Node *ptr;
  List(const Node *_ptr) : ptr(_ptr) {}
public:
  List() : List((const Node*)0) {}
  List(Nothing) : List() {}
  List(memory::Alloc &A, E h, List<E> t) : List(A.alloc_init(Node{t.ptr,h})) {}
  List(memory::Alloc &A, E h) : List(A,h,List()) {}
  
  bool empty() const { return !ptr; }
  const E head() const {
    DEBUG if(empty()) error("<0>.head()");
    return ptr->head;
  }
  const List<E> tail() const {
    DEBUG if(empty()) error("<0>.tail()");
    return List(ptr->tail);
  }

  void push(memory::Alloc &a, E h){ *this = List(a,h,*this); }
  [[nodiscard]] List add(memory::Alloc &a, E h) const { return List(a,h,*this); }
  size_t size() const { return empty() ? 0 : tail().size()+1; }
};

}  // namespace tableau

#endif  // MEMORY_LIST_H_ 
