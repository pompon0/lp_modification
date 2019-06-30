#ifndef UTIL_LOG_H_
#define UTIL_LOG_H_

#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/util/short.h"
#include <algorithm>
#include <iostream>
#include <fstream>
#include <ctime>
#include <unistd.h>

namespace util
{
  struct Logger
  {
    enum { INFO = 0, ERROR = 1, PUSH_FRAME = 2, POP_FRAME = 3 };
    virtual void log(int level, str msg){}
    static void insert(Logger *l){ L().push_back(l); }
    static void erase(Logger *l)
    { 
      size_t j = 0;
      for(auto *m : L()) if(l!=m) L()[j++] = m;
      L().resize(j);
    }
    static arr<Logger*>& L(){ static arr<Logger*> l; return l; }
  };

  ///////////////////////////////////////////////////////////////////////

  /** INFO log */
  template<typename ...Args> inline void info(const str &s, Args ...args)
  {
    auto msg = fmt(s,args...);
    for(auto *l : Logger::L()) l->log(Logger::INFO,msg);
  }

  /** ERROR log */
  template<typename ...Args> [[noreturn]] inline void error(str s, Args ...args)
  {
    auto msg = fmt(s,args...);
    for(auto *l : Logger::L()) l->log(Logger::ERROR,msg);
    _exit(EXIT_FAILURE);
  }

  /** RAII of a stack frame */
  struct Frame
  {
    bool verbose;
    str msg;
    template<typename ...Arg> Frame(bool _verbose, str format_str, Arg ...arg)
    {
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
  inline str now(str tf = "%F %T ")
  {
    char B[25]; time_t t; time(&t);
    strftime(B,sizeof B,tf.c_str(),localtime(&t));
    return B;
  }

  struct StreamLogger : Logger
  {
    std::ostream &os;
    StreamLogger(std::ostream &_os) : os(_os) { Logger::insert(this); }
    ~StreamLogger(){ Logger::erase(this); }
    arr<str> stack;

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
  struct FileLogger : Logger
  {
    FileLogger(){ Logger::insert(this); }
    ~FileLogger(){ Logger::erase(this); close(); }
    arr<str> stack;

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
}

#endif  // LOG_H_
