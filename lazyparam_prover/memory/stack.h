#ifndef MEMORY_STACK_H_
#define MEMORY_STACK_H_

#include "lazyparam_prover/types.h"
#include "lazyparam_prover/log.h"

namespace memory {

template<uint32_t BLOCK> struct Stack {
public:
  struct Save {
    size_t blocks_used;
    u8 *begin,*end;
  };
private:
  vec<u8*> blocks;
  Save state = {
    .blocks_used = 0,
    .begin = 0,
    .end = 0,
  };
  Stack(const Stack &) = delete;
public:
  Stack(){}
  ~Stack(){ for(auto b : blocks) delete[](b); }
  
  Save save() { return state; }
  void restore(Save _state){ state = _state; }

  inline u8* alloc_bytes(size_t s) {
    COUNTER("alloc_bytes");
    DEBUG if(s>BLOCK) error("% = s > BLOCK = %",s,BLOCK);
    if(state.begin+s>state.end){
      if(blocks.size()==state.blocks_used){
        COUNTER("allocated_blocks");
        blocks.push_back(new u8[BLOCK]);
      }
      state.begin = blocks[state.blocks_used++];
      state.end = state.begin+BLOCK;
    }
    auto ptr = state.begin; state.begin += s; return ptr;
  }

  template<typename T> inline T* alloc_init(const T &v) {
    COUNTER("alloc_init");
    return new(alloc_bytes(sizeof(T)))T(v);
  }
};

using Alloc = Stack<(1<<20)>;

} // namespace memory


#endif // MEMORY_STACK_H_
