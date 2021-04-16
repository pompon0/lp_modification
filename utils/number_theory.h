#ifndef UTILS_NUMBER_THEORY_H_
#define UTILS_NUMBER_THEORY_H_

namespace util {

template<typename T> constexpr T pow(T a, T b) {
  T res = 1;
  while(b) {
    if(b&1) res *= a;
    a *= a;
    b >>= 1;
  }
  return res;
}

// inv(a)*a % (1<<bits(T)) = 1
// T is an unsigned integer type.
template<typename T> constexpr T inv(T a) {
  static_assert(T(-1)>T(0));
  if(!a%2) return 0; // TODO: throw error
  // By Fermat's little theorem:
  // a^totient(n) = 1 (%n)
  // a^{-1} = a^{totient(n)-1}
  //
  // For n = 1<<bits(T) we have totient(n) = n/2
  T tot = T(1)<<(sizeof(T)*8-1);
  return pow<T>(a,tot-T(1));
}

}  // namespace utils

#endif  // UTILS_NUMBER_THEORY_H_
