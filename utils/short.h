#ifndef UTIL_SHORT_H_
#define UTIL_SHORT_H_

#include <algorithm>
#include "utils/types.h"

namespace util
{
  /** @brief floor(log_2(x)) */
  inline size_t bits(int x){ return __builtin_popcount(x); }
  inline size_t flog(int x){ return 8*sizeof(int)-1-__builtin_clz(x); }
  template<typename T> inline void mini(T &a, T b){ if(b<a) a = b; }
  template<typename T> inline void maxi(T &a, T b){ if(a<b) a = b; }
  // std::abs is not in standard (standard section 26.8.7-9)
  template<typename T> inline T abs(T a){ return a<0?-a:a; }
  template<typename T> int sign(T a){ return (a>0)-(a<0); }

  // M_PI and M_E are not in c++ standard (only in posix)
  // 3.141592653589793238462643383279502884197169399375105820974944
  // 2.718281828459045235360287471352662497757247093699959574966967

  template<typename T, typename Cmp> inline vec<T> merge(
    const vec<T> &x, const vec<T> &y, Cmp cmp)
  {
    vec<T> z(x.size()+y.size());
    std::merge(x.begin(),x.end(),y.begin(),y.end(),z.begin(),cmp);
    return z;
  }

  template<typename T> inline void unique(vec<T> &v)
  {
    std::sort(v.begin(),v.end());
    v.erase(std::unique(v.begin(),v.end()),v.end());
  }
  template<typename T> inline vec<T> reverse(const vec<T> &A){ return vec<T>(A.rbegin(),A.rend()); }
}

#endif  // UTIL_SHORT_H_

