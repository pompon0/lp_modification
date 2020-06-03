#ifndef MEMORY_LAZY_H_
#define MEMORY_LAZY_H_

#include "lazyparam_prover/memory/alloc.h"

namespace tableau {

template<typename R> struct Lazy {
  struct Impl {
    using Res = R;
    virtual Res get() const = 0;
  };
  R get() const { return impl->get(); }
  explicit Lazy(const Impl *_impl) : impl(_impl) {}
private:
  const Impl *impl;
};

template<typename T> Lazy<typename T::Res> lazy(const T &t) {
  return Lazy<typename T::Res>(alloc_init(t));
}

}  // namespace tableau

#endif  // MEMORY_LAZY_H_
