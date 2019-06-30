#ifndef LOG_H_
#define LOG_H_

#include "lazyparam_prover/util/log.h"

#ifndef VERBOSE
  #define VERBOSE 0
#endif

#ifdef DEBUG
  #define FRAME(args...) util::Frame _(VERBOSE,args);
#elif
  #define FRAME(args...) 
#endif

using util::error;
using util::info;
using util::StreamLogger;

#endif // LOG_H_
