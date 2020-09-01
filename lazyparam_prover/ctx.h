#ifndef CTX_H_
#define CTX_H_

#include <mutex>
#include <future>
#include <memory>
#include <set>
#include "absl/time/time.h"
#include "lazyparam_prover/log.h"

struct Defer {
  Defer(std::function<void()> _f) : f(_f) {}
  ~Defer(){ f(); }
  std::function<void()> f;
};

struct Ctx {
  using Ptr = std::shared_ptr<Ctx>;
  using Cancel = std::function<void()>;
  using Lock = std::lock_guard<std::mutex>; 

  bool done() const { FRAME("done()"); return f.wait_for(std::chrono::seconds(0))==std::future_status::ready; }
  void wait() const { FRAME("wait()"); f.wait(); } 
  ~Ctx() { FRAME("~Ctx()");
    if(base) base->del_child(this);
  }
  static Ptr background();
  static std::tuple<Ptr,Cancel> with_cancel(Ptr base);
  static std::tuple<Ptr,Cancel> with_timeout(Ptr base, absl::Duration timeout);
private:
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
  std::future<void> extra_future;
  std::shared_future<void> f;
  std::set<Ctx*> children;
  Ptr base;
  void add_child(Ctx *child) { FRAME("add_child"); Lock L(mtx); children.insert(child); }
  void del_child(Ctx *child) { FRAME("del_child"); Lock L(mtx); children.erase(child); }
};

Ctx::Ptr Ctx::background() {
  return Ptr(new Ctx());
}

std::tuple<Ctx::Ptr,Ctx::Cancel> Ctx::with_cancel(Ctx::Ptr base) {
  Ptr ctx(new Ctx(base));
  return std::make_tuple(ctx,[ctx]{ ctx->cancel(); });
}

std::tuple<Ctx::Ptr,Ctx::Cancel> Ctx::with_timeout(Ctx::Ptr base, absl::Duration timeout) {
  Ptr ctx(new Ctx(base));
  ctx->extra_future = std::async(std::launch::async,[timeout,ctx]{
    ctx->f.wait_for(absl::ToChronoMilliseconds(timeout));
    ctx->cancel();
  });
  return std::make_tuple(ctx,[ctx]{ ctx->cancel(); });
}
#endif  // CTX_H_
