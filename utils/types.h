#ifndef UTILS_TYPES_H_
#define UTILS_TYPES_H_

#include <cstdint>
#include <vector>
#include <memory>
#include <string>
#include <algorithm>

using u8 = uint8_t;
using u64 = uint64_t;
using s64 = int64_t;
template<typename T> using vec = std::vector<T>;
template<typename T> using ptr = std::unique_ptr<T>;
template<typename T> inline ptr<T> own(T *v){ return ptr<T>(v); }
template<typename T, typename ...Args> inline ptr<T> make(Args&& ...args){ return own(new T(args...)); }
using str = std::string;
using std::swap;

using Byte = uint8_t;
using Bytes = vec<Byte>;

struct Nil {
  template<typename T> operator ptr<T>(){ return {}; }
};
constexpr Nil nil;

#endif // UTILS_TYPES_H_
