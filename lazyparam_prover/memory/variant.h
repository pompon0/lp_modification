#ifndef MEMORY_VARIANT_H_
#define MEMORY_VARIANT_H_

namespace memory {

template<typename T, size_t OFFSET> struct Lens {
  enum { BEGIN = OFFSET, END = BEGIN+sizeof(T) };
  INL static T* at(uint8_t *ptr){ return (T*)(ptr+BEGIN); }
};

template<typename Sum, size_t V, typename T> struct Variant {
private:
  Sum sum;
  using LValue = Lens<T,Sum::SIZE>;
  enum { SIZE = LValue::END };
public:
  INL explicit Variant(Sum _sum) : sum(_sum) {
    DEBUG if(sum.type()!=V) error("type() = %, want %",sum.type(),V);
  }
  INL const T& operator*() const { return *LValue::at(sum.ptr); }
  INL const T* operator->() const { return LValue::at(sum.ptr); }
  INL explicit operator Sum() const { return sum; }

  struct Builder {
    uint8_t *ptr;
    INL explicit Builder(memory::Alloc &a) : ptr(a.alloc_bytes(SIZE)) { *Sum::LType::at(ptr) = V; }
    INL T& operator*(){ return *LValue::at(ptr); }
    INL T* operator->(){ return LValue::at(ptr); }
    INL Variant build(){ return Variant(Sum(ptr)); }
  };
};

}  // namespace tableau

#endif  // MEMORY_VARIANT_H_
