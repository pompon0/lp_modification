#ifndef LOG_H_
#define LOG_H_

#include "lazyparam_prover/util/log.h"
#include "lazyparam_prover/util/time.h"
#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/types.h"
#include <map>

struct Profile {
  struct Scope { size_t count = 0; double time = 0; };
  struct Visit {
    Visit(Scope &_scope) : scope(_scope), start_time(util::realtime_sec()) {}
    ~Visit(){ scope.count++; scope.time += util::realtime_sec()-start_time; } 
    Scope &scope;
    double start_time;
  };
  std::map<str,Scope> scopes;
  str show() {
    vec<str> lines;
    for(auto p : scopes) lines.push_back(util::fmt("% : % : %\n",p.first,p.second.count,p.second.time));
    return util::join("",lines);
  }
};

extern Profile profile;

#ifdef PROFILE
  #define SCOPE(name) static Profile::Scope &_scope = profile.scopes[name]; Profile::Visit _visit(_scope);
  #define COUNTER(name) { static Profile::Scope &_scope = profile.scopes[name]; _scope.count++; }
#else
  #define SCOPE(name)
  #define COUNTER(name)
#endif

#ifdef VERBOSE
  #define VERBOSE 1
#else
  #define VERBOSE 0
#endif

#ifdef DEBUG_MODE
  #define DEBUG if(1)
  #define DEBUG_ONLY(args...) args
  #define FRAME(args...) util::Frame _(VERBOSE,args);
#else
  #define DEBUG if(0)
  #define DEBUG_ONLY(args...)
  #define FRAME(args...) 
#endif

using util::error;
using util::info;
using util::StreamLogger;

#endif // LOG_H_
