#ifndef UTILS_LOG_H_
#define UTILS_LOG_H_

#include "utils/string.h"
#include "utils/short.h"
#include "utils/log.h"
#include "utils/string.h"
#include "utils/types.h"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <ctime>
#include <unistd.h>
#include <errno.h>
#include <map>

#define INL [[gnu::always_inline]]
#define INLL __attribute__((always_inline))
namespace util {

struct Logger {
  enum { INFO = 0, ERROR = 1, PUSH_FRAME = 2, POP_FRAME = 3 };
  virtual void log(int level, str msg){}
  static void insert(Logger *l){ L().push_back(l); }
  static void erase(Logger *l) { 
    size_t j = 0;
    for(auto *m : L()) if(l!=m) L()[j++] = m;
    L().resize(j);
  }
  static vec<Logger*>& L(){ static vec<Logger*> l; return l; }
};

///////////////////////////////////////////////////////////////////////

/** INFO log */
template<typename ...Args> inline void info(const str &s, Args ...args) {
  auto msg = fmt(s,args...);
  for(auto *l : Logger::L()) l->log(Logger::INFO,msg);
}

/** ERROR log */
template<typename ...Args> [[noreturn]] void error(str s, Args ...args) {
  auto msg = fmt(s,args...);
  for(auto *l : Logger::L()) l->log(Logger::ERROR,msg);
  _exit(EXIT_FAILURE);
}

/** RAII of a stack frame */
struct Frame {
  bool verbose;
  str msg;
  template<typename ...Arg> Frame(bool _verbose, str format_str, Arg ...arg) {
    verbose = _verbose;
    msg = fmt(format_str,arg...); 
    for(auto *l : Logger::L()) {
      if(verbose) l->log(Logger::INFO,fmt("% BEGIN",msg));
      l->log(Logger::PUSH_FRAME,msg);
    }
  }
  ~Frame() {
    for(auto *l : Logger::L()) {
      if(verbose) l->log(Logger::INFO,fmt("% END",msg));
      l->log(Logger::POP_FRAME,"");
    }
  }
};

///////////////////////////////////////////////////////////////////////

/** @return timestamp in tf format */
inline str now(str tf = "%F %T ") {
  char B[25]; time_t t; time(&t);
  strftime(B,sizeof B,tf.c_str(),localtime(&t));
  return B;
}

struct StreamLogger : Logger {
  std::ostream &os;
  StreamLogger(std::ostream &_os) : os(_os) { Logger::insert(this); }
  ~StreamLogger(){ Logger::erase(this); }
  vec<str> stack;

  void log(int level, str msg)
  {
    switch(level)
    {
      case INFO: os << now() << msg << std::endl; break;
      case PUSH_FRAME: stack.push_back(msg); break;
      case POP_FRAME: stack.pop_back(); break;
      case ERROR:
        msg = fmt("\n%\nERROR: %\n",join("\n",stack),msg);
        os << msg << std::flush;
        break;
    }
  }
};

/** Logger logging to an arbitrary file */
struct FileLogger : Logger {
  FileLogger(){ Logger::insert(this); }
  ~FileLogger(){ Logger::erase(this); close(); }
  vec<str> stack;

  /** @param filename - output file path */
  void open(const char *filename)
  { close(); file.open(filename,std::ios::app); file << now() << " == START == " << std::endl; }
  void close(){ file << now() << " == STOP == " << std::endl; file.close(); }
  void log(int level, str msg)
  {
    switch(level)
    {
      case INFO: file << now() << msg << std::endl; break;
      case PUSH_FRAME: stack.push_back(msg); break;
      case POP_FRAME: stack.pop_back(); break;
      case ERROR:
        msg = fmt("\n%\nERROR: %\n",join("\n",reverse(stack)),msg);
        file << msg << std::flush;
        break;
    }
  }
  std::ofstream file;
};

} // util

using util::error;
using util::info;
using util::StreamLogger;

static inline double realtime_sec() {
  timespec tv;
  if(clock_gettime(CLOCK_REALTIME, &tv)==-1)
    error("%",strerror(errno));
  return tv.tv_sec + tv.tv_nsec * 1e-9;
}

struct Profile {
  struct Scope { size_t count = 0; double time = 0; };
  struct Visit {
    Visit(Scope &_scope) : scope(_scope), start_time(realtime_sec()) {}
    ~Visit(){ scope.count++; scope.time += realtime_sec()-start_time; } 
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

#endif // UTILS_LOG_H_
