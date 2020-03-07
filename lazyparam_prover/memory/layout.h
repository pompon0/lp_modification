#ifndef MEMORY_LAYOUT_H_
#define MEMORY_LAYOUT_H_

#include "lazyparam_prover/memory/alloc.h"
#include "lazyparam_prover/types.h"

namespace tableau {

struct Empty { enum { SIZE = 0 }; };

template<typename T, typename Prev = Empty> struct Field {
  enum { SIZE = Prev::SIZE+sizeof(T) };
  static T& ref(u8 *ptr){ return *(T*)(ptr+Prev::SIZE); }
  static u8* alloc(){ return alloc_bytes(SIZE); }
};

template<typename T, typename Prev = Empty> struct ArrayField {
  using SizeField = Field<size_t,Prev>;
  static size_t size(u8 *ptr){ return SizeField::ref(ptr); }
  static u8* alloc(size_t n){
    auto p = alloc_bytes(SizeField::SIZE+n*sizeof(T));
    SizeField::ref(p) = n;
    return p;
  }
  static T& ref(u8 *ptr, size_t i) {
    DEBUG if(i>=size(ptr)) error("<size=%>.ref(%)",size(ptr),i);
    return ((T*)(ptr+SizeField::SIZE))[i];
  }
};

}  // namespace tableau

#endif  // MEMORY_LAYOUT_H_


