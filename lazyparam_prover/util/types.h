#ifndef UTIL_TYPES_H_
#define UTIL_TYPES_H_

#include <vector>
#include <string>
#include <memory>
#include <cstdint>

namespace util
{
  using str = std::string;
  template<typename T> using arr = std::vector<T>;
  template<typename T> using ptr = std::unique_ptr<T>;
  template<typename T> inline ptr<T> own(T *v){ return ptr<T>(v); }
  typedef uint8_t Byte;
  typedef arr<Byte> Bytes;

  struct Nil {
    template<typename T> operator ptr<T>(){ return {}; }
  };
  constexpr Nil nil;
  template<typename T> static inline ptr<T> just(const T &v){ return ptr<T>(new T(v)); }
}

#endif  // UTIL_TYPES_H_
