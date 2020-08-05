#ifndef MEMORY_LAZY_H_
#define MEMORY_LAZY_H_

#include "lazyparam_prover/memory/stack.h"

namespace tableau {

template<typename R> struct Lazy {
  struct Impl {
    using Res = R;
    virtual Res get(memory::Alloc &A) const = 0;
  };
  R get(memory::Alloc &A) const { return impl->get(A); }
  explicit Lazy(const Impl *_impl) : impl(_impl) {}
private:
  const Impl *impl;
};

template<typename T> Lazy<typename T::Res> lazy(memory::Alloc &A, const T &t) {
  return Lazy<typename T::Res>(A.alloc_init(t));
}

}  // namespace tableau

#endif  // MEMORY_LAZY_H_
