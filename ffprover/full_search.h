#ifndef FFPROVER_FULL_SEARCH_H_
#define FFPROVER_FULL_SEARCH_H_

#include "ffprover/search.h"
#include "ffprover/tree.h"

namespace ff {

struct FullSearch {
  vec<size_t> path;
  Result::Stats stats;

  bool dfs(Ctx::Ptr ctx, size_t depth, controller::Prover &p) {
    if(p.done()) return 1;
    auto s = p.save();
    for(size_t i=p.action_count(); i--;) {
      if(ctx->done()) return 0;
      p.restore(s);
      if(p.action_features(i).path_length>depth) continue;
      stats.inferences++;
      p.apply_action(i);
      if(dfs(ctx,depth,p)) {
        path.push_back(i);
        return 1;
      }
    }
    return 0;
  }

  // This is an iterative_deepening strategy bounded by PROOF DEPTH, not MCTS TREE DEPTH.
  Result run(Ctx::Ptr ctx, Tree::Ptr t, controller::Prover &p) { FRAME("FullSearch::run");
    auto s = p.save();
    for(size_t depth=1;; depth++) {
      p.restore(s);
      info("depth = %",depth);
      if(dfs(ctx,depth,p)) {
        p.restore(s);
        while(!p.done()) { 
          size_t ac = p.action_count();
          t.expand(vec<double>(ac));
          t = t.child(path.back());
          p.apply_action(path.back());
          path.pop_back();
        }
        t.visit(1);
        t.set_won();
        stats.bigsteps = depth;
        return {
          .status = Result::SOLVED,
          .node = t,
          .stats = stats,
        };
      }
      if(ctx->done()) {
        stats.bigsteps = depth;
        return {
          .status = Result::CANCELLED,
          .node = t,
          .stats = stats,
        };
      }
    }
  }
};

}  // namespace ff

#endif
