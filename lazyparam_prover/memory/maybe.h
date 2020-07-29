#ifndef MEMORY_MAYBE_H_
#define MEMORY_MAYBE_H_

#include "lazyparam_prover/log.h"

namespace tableau {

struct Nothing {};
template<typename T> struct Maybe {
private:
  u8 data[sizeof(T)];
  bool present;
public:
  Maybe() : present(0) {}
  Maybe(Nothing) : Maybe() {}
  explicit Maybe(const T &v) : present(1) { new(data)T(v); }
  explicit operator bool() const { return present; } 
  const T& get() const {
    DEBUG if(!present) error("Maybe::get(): not present");
    return *(T*)data;
  }
  bool operator==(Maybe<T> m) const {
    return present == m.present && (!present || get()==m.get());
  }
};

static inline Nothing nothing(){ return {}; }
template<typename T> static inline Maybe<T> just(const T &v) { return Maybe<T>(v); }

}  // namespace tableau

#endif  // MEMORY_MAYBE_H_
