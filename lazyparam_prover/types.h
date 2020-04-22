#ifndef TYPES_H_
#define TYPES_H_

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
using str = std::string;
using std::swap;

#endif // TYPES_H_
