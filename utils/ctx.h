#ifndef CTX_H_
#define CTX_H_

#include <mutex>
#include <future>
#include <memory>
#include <set>
#include "absl/time/time.h"
#include "utils/log.h"

struct Ctx {
  using Ptr = std::shared_ptr<Ctx>;
  using Cancel = std::function<void()>;
  using Lock = std::lock_guard<std::mutex>; 
  bool done() const { FRAME("done()"); return f.wait_for(std::chrono::seconds(0))==std::future_status::ready; }
  void wait() const { FRAME("wait()"); f.wait(); } 
  virtual ~Ctx() {
    if(base) base->del_child(this);
  }
  static Ptr background(){ return Ptr(new Ctx()); }
  static std::tuple<Ptr,Cancel> with_cancel(Ptr base) {
    Ptr ctx(new Ctx(base));
    return std::make_tuple(ctx,[ctx]{ ctx->cancel(); });
  }
  static Ptr with_timeout(Ptr base, absl::Duration timeout);
protected:
  Ctx(Ptr _base = 0) : base(_base) { FRAME("Ctx()");
    f = p.get_future().share();
    if(base) base->add_child(this);
  }
  void cancel() { FRAME("cancel");
    Lock L(mtx);
    if(done()) return;
    p.set_value();
    for(Ctx *child : children) child->cancel();
  }
  std::mutex mtx;
  std::promise<void> p;
  std::shared_future<void> f;
  std::set<Ctx*> children;
  Ptr base;
  void add_child(Ctx *child) { FRAME("add_child");
    Lock L(mtx);
    children.insert(child);
    if(done()) child->cancel();
  }
  void del_child(Ctx *child) {
    FRAME("del_child");
    Lock L(mtx);
    children.erase(child);
  }
};

struct CtxWithTimeout : Ctx {
  CtxWithTimeout(Ctx::Ptr base, absl::Duration timeout) : Ctx(base) {
    timeout_task = std::async(std::launch::async,[this,timeout]{
      // TODO: probably wait_until would be better, as there are no guarantees
      // on when the async task is started.
      f.wait_for(absl::ToChronoMilliseconds(timeout));
      cancel();
    });
  }
  ~CtxWithTimeout() {
    cancel();
    timeout_task.wait();
  }
private:
  std::future<void> timeout_task;
};

Ctx::Ptr Ctx::with_timeout(Ctx::Ptr base, absl::Duration timeout) {
  return Ptr(new CtxWithTimeout(base,timeout));
}
#endif  // CTX_H_
