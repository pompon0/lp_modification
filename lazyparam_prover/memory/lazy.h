#ifndef MEMORY_LAZY_H_
#define MEMORY_LAZY_H_

#include "lazyparam_prover/memory/stack.h"

namespace memory {

template<typename R> struct Lazy {
  struct Impl {
    using Res = R;
    virtual Res get(memory::Alloc &A) const = 0;
  };
  INL R get(memory::Alloc &A) const { return impl->get(A); }
  INL explicit Lazy(const Impl *_impl) : impl(_impl) {}
private:
  const Impl *impl;
};

template<typename T> INL inline Lazy<typename T::Res> lazy(memory::Alloc &A, const T &t) {
  return Lazy<typename T::Res>(A.alloc_init(t));
}

}  // namespace tableau

#endif  // MEMORY_LAZY_H_
