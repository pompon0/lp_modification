#ifndef MEMORY_LAZY_H_
#define MEMORY_LAZY_H_

template<typename T> struct Lazy {
  struct Impl { virtual T get() const = 0; };
  T get() const { return impl->get(); }
  Lazy(const Impl *_impl) : impl(_impl) {}
private:
  const Impl *impl;
};

#endif  // MEMORY_LAZY_H_
