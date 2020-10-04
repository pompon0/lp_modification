#ifndef MEMORY_MAYBE_H_
#define MEMORY_MAYBE_H_

#include "utils/log.h"

namespace memory {

struct Nothing {};
template<typename T> struct Maybe {
private:
  u8 data[sizeof(T)];
  bool present;
public:
  INL Maybe() : present(0) {}
  INL Maybe(Nothing) : Maybe() {}
  INL explicit Maybe(const T &v) : present(1) { new(data)T(v); }
  INL explicit operator bool() const { return present; } 
  INL const T& get() const {
    DEBUG if(!present) error("Maybe::get(): not present");
    return *(T*)data;
  }
  INL bool operator==(Maybe<T> m) const {
    return present == m.present && (!present || get()==m.get());
  }
};

INL static inline Nothing nothing(){ return {}; }
template<typename T> INL static inline Maybe<T> just(const T &v) { return Maybe<T>(v); }

}  // namespace memory

#endif  // MEMORY_MAYBE_H_
