#ifndef TYPES_H_
#define TYPES_H_

#include <cstdint>
#include <vector>
#include <memory>
#include <string>

using u8 = uint8_t;
using u64 = uint64_t;
template<typename T> using vec = std::vector<T>;
template<typename T> using ptr = std::unique_ptr<T>;
using str = std::string;

template<typename T> inline void swap(T &a, T &b) { T c = a; a = b; b = c; }

#endif // TYPES_H_
