#ifndef MEMORY_LAYOUT_H_
#define MEMORY_LAYOUT_H_

#include "utils/types.h"
#include "utils/log.h"
#include <algorithm>

namespace memory {

struct Empty {
  u8 *ptr;
  INL explicit Empty(u8 *_ptr = 0) : ptr(_ptr) {}
protected:
  enum { SIZE = 0 };
  INL void init(){}
};

template<typename T, typename Prev = Empty> struct Field : Prev {
  INL Field(u8 *_ptr = 0) : Prev(_ptr) {}
  INL T& ref(){ return *(T*)(Prev::ptr+Prev::SIZE); }
  template<typename Alloc> INL static Field alloc(Alloc &a){
    Field p(a.alloc_bytes(SIZE));
    p.init();
    return p;
  }
protected:
  enum { SIZE = Prev::SIZE+sizeof(T) };
};

template<typename T, typename Prev = Empty> struct ArrayField : Prev {
  INL ArrayField(){}
  INL size_t size() const { return SizeField(Prev::ptr).ref(); }
  template<typename Alloc> INL static ArrayField alloc(Alloc &a, size_t n){
    ArrayField p(a.alloc_bytes(SizeField::SIZE+n*sizeof(T)));
    SizeField(p.ptr).init();
    SizeField(p.ptr).ref() = n;
    return p;
  }
  INL T& ref(size_t i) {
    DEBUG if(i>=size()) error("<size=%>.ref(%)",size(),i);
    return ((T*)(Prev::ptr+SizeField::SIZE))[i];
  }
protected:
  INL explicit ArrayField(u8 *_ptr) : Prev(_ptr) {}
  struct SizeField : Field<size_t,Prev> {
    INL explicit SizeField(u8 *_ptr) : Field<size_t,Prev>(_ptr) {}
    friend struct ArrayField;
  };
};

// experimental

template<typename T, typename Prev = Empty> struct ConstField : Prev {
  INL explicit ConstField(u8 *_ptr = 0) : Prev(_ptr) {}
  INL T ref(){ return BaseField(Prev::ptr).ref(); }
protected:
  struct BaseField : Field<T,Prev> { using Field<T,Prev>::Field; friend struct ConstField; };
  enum { SIZE = BaseField::SIZE };
  INL void init(T val){
    BaseField(Prev::ptr).init();
    BaseField(Prev::ptr).ref() = val;
  }
};
template<typename T, T Val, typename Prev = Empty> struct FixedField : ConstField<T,Prev> {
  INL explicit FixedField(u8 *_ptr = 0) : ConstField<T,Prev>(_ptr) {}
  INL constexpr T ref(){ return Val; }
protected:
  INL void init(){ ConstField<T,Prev>::init(Val); }
};

template<size_t I, typename T> using Variant = Field<T,FixedField<size_t,I>>;

template<typename V> struct VariantWrap;
template<size_t I, typename T> struct VariantWrap<Variant<I,T>> {
  enum { ID = I };
  INL static void to(Variant<I,T> &v, ConstField<size_t> f) {
    if(f.ref()!=I) error("cast failed");
    v = Variant<I,T>(f.ptr);
  }
  INL static ConstField<size_t> from(Variant<I,T> v){ return v; }
};

template<size_t ...I> constexpr bool ascending() {
  size_t A[] = {I...};
  for(size_t i=1; i<sizeof...(I); i++) {
    if(A[i-1]>=A[i]) return false;
  }
  return true;
}

template<typename ...Variants> struct VariantSet : VariantWrap<Variants>... {
  static_assert(ascending<VariantWrap<Variants>::ID...>());
  using VariantWrap<Variants>::from...;
  using VariantWrap<Variants>::to...;
};


struct EmptyVariants { enum { SIZE = 0 }; void from(){} void to(){} };
template<typename H, typename Prev = EmptyVariants> struct Variants : Prev {
  enum { SIZE = Prev::SIZE+1 };
  using Prev::from;
  using Prev::to;
  
  using X = Field<H,FixedField<size_t,SIZE-1>>;
  INL static void to(X &v, ConstField<size_t> f){
    }
  INL static ConstField<size_t> from(X v){ return v; }
};


template<typename ...Variants> struct Coprod  {
  template<typename X> INL explicit Coprod(X x) : type(VariantSet<Variants...>::from(x)) {}
  // TODO: this can be inherited to get rid of variable x 
  template<typename X> INL operator X(){ X x; VariantSet<Variants...>::to(x,type); return x; }
private:
  ConstField<size_t> type;
};


/*struct EmptyVariants { enum { SIZE = 0 }; void from(){} void to(){} };
template<typename H, typename Prev = EmptyVariants> struct Variants : Prev {
  enum { SIZE = Prev::SIZE+1 };
  using Prev::from;
  using Prev::to;
  
  using X = Field<H,FixedField<size_t,SIZE-1>>;
  INL static void to(X &v, ConstField<size_t> f){
    if(f.ref()!=SIZE-1) error("cast failed");
    v = X(f.ptr);
  }
  INL static ConstField<size_t> from(X v){ return v; }
};

struct EmptyCoprod { using Variants = EmptyVariants; };
template<typename T, typename Prev = EmptyCoprod> struct Coprod {
private:
  ConstField<size_t> type;
public:
  using Variants = Variants<T,typename Prev::Variants>;
  using Variant = typename Variants::X;
  template<typename X> INL explicit Coprod(X x) : type(Variants::from(x)) {}
  // TODO: this can be inherited to get rid of variable x 
  template<typename X> INL operator X(){ X x; Variants::to(x,type); return x; }
};*/

/*template<typename T, typename Prev> struct Variant {
  using TypeField = Field<size_t,TypeField>;
  using ValueField = Field<T,TypeField>;
  struct Ptr : TypeField::Ptr {
    Ptr(ValueField::Ptr p){ ptr = p.ptr; }
    Ptr(Prev::Ptr p){ ptr = p.ptr; }
  };
  enum { VARIANT_COUNT = Prev::VARIANT_COUNT+1 };
  template<typename Alloc> static ValueField::Ptr alloc(Alloc &a) {
    auto p = ValueField::alloc(a);
    TypeField::ref(p) = VARIANT_COUNT-1;
  }
  
  template<typename F, typename ...FT> res<F> run(Ptr p, FT ...ft, F f) {
    if(TypeField::ref(p)==VARIANT_COUNT-1) return f();
    return Prev::run(Prev::Ptr{p.ptr},ft...);
  }
};*/

}  // namespace tableau

#endif  // MEMORY_LAYOUT_H_


