#ifndef MEMORY_STACK_H_
#define MEMORY_STACK_H_

#include "utils/types.h"
#include "utils/log.h"

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
  INL Stack(){}
  INL ~Stack(){ for(auto b : blocks) delete[](b); }
  
  INL Save save() { return state; }
  INL void restore(Save _state){ state = _state; }

  INL u8* alloc_bytes(size_t s) {
    PROF_COUNT("memory::alloc_bytes");
    DEBUG if(s>BLOCK) error("% = s > BLOCK = %",s,BLOCK);
    if(state.begin+s>state.end){
      if(blocks.size()==state.blocks_used){
        PROF_COUNT("memory::allocated_blocks");
        blocks.push_back(new u8[BLOCK]);
      }
      state.begin = blocks[state.blocks_used++];
      state.end = state.begin+BLOCK;
    }
    auto ptr = state.begin; state.begin += s; return ptr;
  }

  template<typename T> INL T* alloc_init(const T &v) {
    PROF_COUNT("memory::alloc_init");
    return new(alloc_bytes(sizeof(T)))T(v);
  }
  
  // it is not that easy.
  // TODO: carefully analyze the lifecycle of objects created
  // with this allocator.
  /*
  template<typename T> struct StdAllocator {
    // implicit coercion
    StdAllocator(Stack &_s) : s(&_s) {}
    using value_type = T;
    T *allocate(size_t n){ return (T*)(s->alloc_bytes(n*sizeof(T))); }
    void deallocate(T*,size_t){}
  private:
    Stack *s;
  };*/
};

using Alloc = Stack<(1<<26)>;


} // namespace memory


#endif // MEMORY_STACK_H_
