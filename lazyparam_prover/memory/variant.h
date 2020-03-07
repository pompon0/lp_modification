#ifndef MEMORY_VARIANT_H_
#define MEMORY_VARIANT_H_

namespace tableau {

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
  const T& operator*() const { return *LValue::at(sum.ptr); }
  const T* operator->() const { return LValue::at(sum.ptr); }
  explicit operator Sum() const { return sum; }


  struct Builder {
    uint8_t *ptr;
    Builder() : ptr(alloc_bytes(SIZE)) { *Sum::LType::at(ptr) = V; }
    T& operator*(){ return *LValue::at(ptr); }
    T* operator->(){ return LValue::at(ptr); }
    Variant build(){ return Variant(Sum(ptr)); }
  };
};

}  // namespace tableau

#endif  // MEMORY_VARIANT_H_
