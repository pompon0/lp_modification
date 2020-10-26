#ifndef SYS_H_
#define SYS_H_

#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <cstring>

#include "utils/log.h"
#include "utils/ctx.h"

namespace utils::sys {

// TODO: consider making these shared/unique pointers, so that system resources
// are not leaked. It has to cooperate well with fork() and pipe lifecycle.
struct descriptor {
  int fd;
  enum { invalid = -1 };
  descriptor(int _fd = invalid) : fd(_fd) {}
  
  void close() { //TODO: make nothrow - otherwise it cannot by used in RAII
    if(fd==invalid) return;
    if(::close(fd)==-1){} // error("%",strerror(errno));
    fd = invalid;
  }
};

struct pipe {
  descriptor in,out;
  
  // blocking until all is written
  void write(const str &data) {
    auto b = &data[0];
    auto e = b+data.size();
    ssize_t s = ::write(in.fd,b,e-b);
    if(s==-1) error("write(): %",strerror(errno));
    if(s!=e-b) error("wrote %/% bytes",s,e-b);
  }
  // blocking until some data available
  str read() {
    vec<char> data(PIPE_BUF);
    ssize_t s = ::read(out.fd,&data[0],data.size());
    if(s==-1){
      if(errno==EPIPE) {
        info("reading closed pipe");
        return "";
      }
      error("read(): %",strerror(errno));
    }
    return str(&data[0],&data[s]);
  }

  void close() {
    in.close();
    out.close();
  }
};

static pipe new_pipe() {
  int f[2];
  if(::pipe(f)==-1) error("pipe(): %",strerror(errno));
  pipe p;
  p.in.fd = f[1]; p.out.fd = f[0];
  return p;
}

static str subprocess(Ctx::Ptr ctx, vec<str> cmd, str input) {
  pipe parent_child = new_pipe();
  pipe child_parent = new_pipe();
  pid_t pid = fork();
  if(pid==-1) error("fork(): %",strerror(errno));
  if(pid==0) { // child
    // redirect stdin and stdout (stderr is propagated)
    if(dup2(parent_child.out.fd,0)==-1) error("dup2(): %",strerror(errno));
    if(dup2(child_parent.in.fd,1)==-1) error("dup2(): %",strerror(errno));
    parent_child.close();
    child_parent.close();
    // execute
    vec<vec<char>> args;
    vec<char*> argv;
    for(size_t i=0; i<cmd.size(); i++){
      args.emplace_back(cmd[i].begin(),cmd[i].end());
      args.back().push_back(0);
      argv.push_back(&args.back()[0]);
    }
    argv.push_back(0);
    if(::execv(&argv[0][0],&argv[0])==-1) error("execv(): %",strerror(errno));
    error("execv() returned");
  }
  // close child ends of pipes, so that pipes will be closed,
  // once subprocess is terminated.
  parent_child.out.close();
  child_parent.in.close();
  // write input asynchronously, until pipe is closed.
  auto twrite = std::async(std::launch::async,[parent_child,input]() mutable { 
    parent_child.write(input);
    parent_child.close();
    info("write complete");
  }); 
  // read output asynchronously, until pipe is closed.
  auto tread = std::async(std::launch::async,[child_parent]() mutable {
    str output;
    while(1) {
      auto buffer = child_parent.read();
      if(buffer.empty()) break;
      output += buffer;
    }
    child_parent.close();
    info("read complete, size = %",output.size());
    return output;
  });
  // wait for subprocess asynchronously.
  Ctx::Cancel cancel;
  std::tie(ctx,cancel) = Ctx::with_cancel(ctx);
  auto tdone = std::async(std::launch::async,[cancel,pid]{
    int status;
    if(waitpid(pid,&status,0)==-1) error("waitpid(%): %",pid,strerror(errno));
    info("subprocess terminated");
    if(WIFEXITED(status)) {
      if(auto s = WEXITSTATUS(status); s==EXIT_SUCCESS) {  
        cancel();
        return;
      } else {
        error("WEXITSTATUS() = %",s);
      }
    } else if(WIFSIGNALED(status)) {
      error("WTERMSIG() = %",WTERMSIG(status));
    } else {
      error("unknown termination reason");
    }
  });
  // wait for the context to finish.
  ctx->wait();
  info("ctx terminated");
  // kill the subprocess if still running.
  if(waitpid(pid,0,WNOHANG)==0) {
    info("killing subprocess");
    if(kill(pid,SIGKILL)==-1) error("kill(%): %",pid,strerror(errno));
  }
  // wait for the tasks
  tdone.get(); // ends since subprocess terminated
  twrite.get(); // ends since the pipe is closed (subprocess terminanted)
  return tread.get(); // ends since the pipe is closed (subprocess terminated)
}

} // namespace sys

#endif  // SYS_H_
