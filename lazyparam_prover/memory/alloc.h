#ifndef MEMORY_ALLOC_H_
#define MEMORY_ALLOC_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/log.h"

namespace tableau {

struct Snapshot {
  size_t blocks_used;
  u8 *begin,*end;
};

//TODO: zero the memory, when rewinding stack in DEBUG mode.
Snapshot stack{0,0,0};

inline u8* alloc_bytes(size_t s) {
  COUNTER("alloc_bytes");
  enum { BLOCK = 1<<26 };
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

}  // namespace tableau

#endif  // MEMORY_ALLOC_H_
