#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <cstdlib>
#include <iostream>
#include "utils/log.h"

using namespace util;

util::StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  if(argc<3) {
    error("argc = %, want >=3",argc);
  }
  rlimit l;
  l.rlim_cur = atoi(argv[1]);
  l.rlim_max = l.rlim_cur;
  if(setrlimit(RLIMIT_AS,&l)==-1) {
    error("setrlimit(): %",strerror(errno));
  }
  if(execv(argv[2],argv+2)==-1) {
    error("execv(%): %",argv[2],strerror(errno));
  }
  return 0;
}
