#ifndef UTIL_TIME_H_
#define UTIL_TIME_H_

#include <ctime>
#include <cstring>
#include <errno.h>
#include "lazyparam_prover/util/log.h"

namespace util
{
  /** @brief wrapper around clock_gettime, providing realtime in seconds */
  static inline double realtime_sec()
  {
    timespec tv;
    if(clock_gettime(CLOCK_REALTIME, &tv)==-1)
      error("%",strerror(errno));
    return tv.tv_sec + tv.tv_nsec * 1e-9;
  }
}

#endif  // UTIL_TIME_H_
