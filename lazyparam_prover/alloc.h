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
  return new u8[s];
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
  explicit Maybe(const T &v) : present(1) { new(data)T(v); }
  explicit operator bool() const { return present; } 
  T get() const {
    DEBUG if(!present) error("Maybe::get(): not present");
    return *(T*)data;
  }
  bool operator==(Maybe<T> m) const {
    return present == m.present && (!present || get()==m.get());
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
private:
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

template<typename E> struct RewindArray {
private:
  vec<Maybe<E>> data;
public:
  void validate_idx(size_t i) const { if(i>=data.size()) error("[0..%) [%]",data.size(),i); }
  const Maybe<E> operator[](size_t i) const { DEBUG validate_idx(i); return data[i]; }
  void resize(size_t n){ data.resize(n); }
  size_t size() const { return data.size(); }
  void set(size_t i, E e){ data[i] = Maybe<E>(e); }
  struct Snapshot { vec<Maybe<E>> data; };
  Snapshot snapshot(){ return {data}; }
  void rewind(const Snapshot &s){ data = s.data; }
};

/*template<typename E> struct RewindArray {
private:
  vec<Maybe<E>> data;
  vec<size_t> diffs;
  void validate_idx(size_t i) const { if(i>=data.size()) error("[0..%) [%]",data.size(),i); }
public:
  const Maybe<E> operator[](size_t i) const { DEBUG validate_idx(i); return data[i]; }
  void resize(size_t n){ FRAME("resize(%)",n);
    DEBUG if(n<data.size()) error("only expanding the array is supported");
    // downsizing can be supported if we use non-downsizable non-relocable storage.
    data.resize(n);
  }
  size_t size() const { return data.size(); }
  void set(size_t i, E e){
    DEBUG validate_idx(i);
    DEBUG if(data[i]) error("data[%] already set",i);
    data[i] = Maybe<E>(e);
    diffs.push_back(i);
  }

  struct Snapshot { size_t data_size, diffs_size; };
  Snapshot snapshot(){ return {data.size(),diffs.size()}; }
  void rewind(Snapshot s){ FRAME("rewind(diffs_size = %, data_size = %)",s.diffs_size,s.data_size);
    DEBUG if(s.diffs_size>diffs.size()) error("s.diffs_size = % > diffs.size() = %",s.diffs_size,diffs.size());
    DEBUG if(s.data_size>data.size()) error("s.data_size = % > data.size() = %",s.data_size,data.size());
    while(diffs.size()>s.diffs_size){ data[diffs.back()] = Maybe<E>(); diffs.pop_back(); }
    data.resize(s.data_size);
  }
};*/

template<typename T, size_t OFFSET> struct Lens {
  enum { BEGIN = OFFSET, END = BEGIN+sizeof(T) };
  static T* at(uint8_t *ptr){ return (T*)(ptr+BEGIN); }
};

template<typename Sum, size_t V, typename T> struct Variant {
private:
  Sum sum;
  using LValue = Lens<T,Sum::SIZE>;
  enum { SIZE = LValue::END };
public:
  explicit Variant(Sum _sum) : sum(_sum) {
    DEBUG if(sum.type()!=V) error("type() = %, want %",sum.type(),V);
  }
  const T* operator->() const { return LValue::at(sum.ptr); }
  explicit operator Sum() const { return sum; }


  struct Builder {
    uint8_t *ptr;
    Builder() : ptr(alloc_bytes(SIZE)) { *Sum::LType::at(ptr) = V; }
    T* operator->(){ return LValue::at(ptr); }
    Variant build(){ return Variant(Sum(ptr)); }
  };
};


#endif // ALLOC_H_
