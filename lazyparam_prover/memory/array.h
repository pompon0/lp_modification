#ifndef MEMORY_ARRAY_H_
#define MEMORY_ARRAY_H_

#include "lazyparam_prover/memory/alloc.h"
#include "lazyparam_prover/memory/maybe.h"
#include "lazyparam_prover/util/log.h"

namespace tableau {

template<typename E> struct Array {
private:
  size_t size_;
  E *ptr;
  void validate_idx(size_t i) const { if(i>=size_) error("[0..%) [%]",size_,i); }
public:
  Array() : size_(0) {}
  Array(const Array &a) : Array(a.size_) { for(size_t i=0; i<size_; ++i) ptr[i] = a.ptr[i]; }
  Array(size_t _size) : size_(_size), ptr((E*)alloc_bytes(size_*sizeof(E))) {}
  size_t size() const { return size_; }
  E & operator[](size_t i){ DEBUG validate_idx(i); return ptr[i]; }
  const E & operator[](size_t i) const { DEBUG validate_idx(i); return ptr[i]; }
};

template<typename E> struct ResetArray {
private:
  vec<E> data;
  vec<size_t> dirty;
  void validate_idx(size_t i) const { if(i>=data.size()) error("[0..%) [%]",data.size(),i); }
public:
  E operator[](size_t i) const { DEBUG validate_idx(i); return data[i]; }
  void resize(size_t n, E e){ FRAME("resize(%,_)",n);
    data.resize(n,e);
  }
  size_t size() const { return data.size(); }
  void set(size_t i, E e){
    DEBUG validate_idx(i);
    data[i] = e;
    dirty.push_back(i);
  }

  void reset(E e){ FRAME("reset()");
    for(auto i : dirty) data[i] = e;
    dirty.clear();
  }
};

template<typename E> struct RewindArray {
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
};

}  // namespace tableau

#endif  // MEMORY_ARRAY_H_
