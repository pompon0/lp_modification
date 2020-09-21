#ifndef MEMORY_LAYOUT_H_
#define MEMORY_LAYOUT_H_

#include "utils/types.h"
#include "utils/log.h"

namespace memory {

struct Empty { enum { SIZE = 0 }; };

template<typename T, typename Prev = Empty> struct Field {
  enum { SIZE = Prev::SIZE+sizeof(T) };
  INL static T& ref(u8 *ptr){ return *(T*)(ptr+Prev::SIZE); }
  template<typename Alloc> INL static u8* alloc(Alloc &a){ return a.alloc_bytes(SIZE); }
};

template<typename T, typename Prev = Empty> struct ArrayField {
  using SizeField = Field<size_t,Prev>;
  INL static size_t size(u8 *ptr){ return SizeField::ref(ptr); }
  template<typename Alloc> INL static u8* alloc(Alloc &a, size_t n){
    auto p = a.alloc_bytes(SizeField::SIZE+n*sizeof(T));
    SizeField::ref(p) = n;
    return p;
  }
  INL static T& ref(u8 *ptr, size_t i) {
    DEBUG if(i>=size(ptr)) error("<size=%>.ref(%)",size(ptr),i);
    return ((T*)(ptr+SizeField::SIZE))[i];
  }
};

}  // namespace tableau

#endif  // MEMORY_LAYOUT_H_


