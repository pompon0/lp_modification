#ifndef CTX_H_
#define CTX_H_

#include <future>
#include <atomic>
#include <memory>
#include "absl/time/time.h"

struct Ctx {
  static std::shared_ptr<Ctx> with_timeout(absl::Duration timeout) {
    std::shared_ptr<Ctx> ctx(new Ctx());
    ctx->future = std::async(std::launch::async,[timeout,ctx]{
      std::this_thread::sleep_for(absl::ToChronoMilliseconds(timeout));
      ctx->done_.store(true);
    });
    return ctx;
  }
  bool done() const { return done_.load(); }
private:
  Ctx() : done_(false) {}
  std::atomic<bool> done_;
  std::future<void> future;
};

#endif  // CTX_H_
