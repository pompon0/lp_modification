#ifndef ALLOC_H_
#define ALLOC_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/log.h"

struct Snapshot {
  size_t blocks_used;
  u8 *begin,*end;
};

//TODO: zero the memory, when rewinding stack in DEBUG mode.
Snapshot stack{0,0,0};

inline u8* alloc_bytes(size_t s) {
  COUNTER("alloc_bytes");
  enum { BLOCK = 1<<22 };
  DEBUG if(s>BLOCK) error("% = s > BLOCK = %",s,BLOCK);
  static vec<u8*> blocks;
  if(stack.begin+s>stack.end){
    if(blocks.size()==stack.blocks_used){
      COUNTER("allocated_blocks");
      blocks.push_back(new u8[BLOCK]);
    }
    stack.begin = blocks[stack.blocks_used++];
    stack.end = stack.begin+BLOCK;
  }
  auto ptr = stack.begin; stack.begin += s; return ptr;
}

inline u64* alloc(size_t s) {
  COUNTER("alloc");
  return (u64*)alloc_bytes(s*sizeof(u64));
}

template<typename T> inline T* alloc_init(const T &v) {
  COUNTER("alloc_init");
  return new(alloc_bytes(sizeof(T)))T(v);
}

template<typename T> struct Maybe {
private:
  u8 data[sizeof(T)];
  bool present;
public:
  Maybe() : present(0) {}
  Maybe(const T &v) : present(1) { new(data)T(v); }
  explicit operator bool() const { return present; } 
  T get() const {
    DEBUG if(!present) error("Maybe::get(): not present");
    return *(T*)data;
  }
};

template<typename E> struct List {
private:
  struct Node { const Node *tail; const E head; };
  const Node *ptr;
  List(const Node *_ptr) : ptr(_ptr) {}
public:
  List() : List(0) {}
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
};

template<typename E> struct Array {
  size_t size_;
  E *ptr;
  void validate_idx(size_t i) const { if(i>=size_) error("[0..%) [%]",size_,i); }
public:
  Array() : size_(0) {}
  Array(size_t _size) : size_(_size), ptr((E*)alloc_bytes(size_*sizeof(E))) {}
  size_t size() const { return size_; }
  E & operator[](size_t i){ DEBUG validate_idx(i); return ptr[i]; }
  const E & operator[](size_t i) const { DEBUG validate_idx(i); return ptr[i]; }
};

#endif // ALLOC_H_
