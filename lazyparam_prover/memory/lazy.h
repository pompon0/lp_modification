#ifndef MEMORY_LAZY_H_
#define MEMORY_LAZY_H_

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

template<typename Alloc, typename T> Lazy<typename T::Res> lazy(Alloc &a, const T &t) {
  return Lazy<typename T::Res>(a.alloc_init(t));
}

}  // namespace tableau

#endif  // MEMORY_LAZY_H_
