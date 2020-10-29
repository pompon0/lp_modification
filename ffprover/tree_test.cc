#define DEBUG_MODE
#include "gtest/gtest.h"

#include "utils/log.h"
#include "ffprover/tree.h"

using namespace ff;

TEST(TREE,simple) {
  StreamLogger _(std::cerr);
  auto t = Tree::New();
  t->root().expand({1,1,1});
  t->root().child(1).expand({1,1});
  t->root().child(1).child(0).set_won();
  EXPECT_EQ(2,t->root().won_depth().get());
  EXPECT_EQ(false,bool(t->root().child(0).won_depth()));
}
