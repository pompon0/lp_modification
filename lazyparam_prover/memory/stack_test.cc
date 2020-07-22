#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/util/log.h"

using namespace memory;

TEST(Stack,simple) {
  StreamLogger _(std::cerr);
  Stack S;
  auto *x = S.alloc_init<int>(5);
  auto save = S.save();
  S.alloc_init<int>(10);
  S.restore(save);
  S.alloc_init<int>(20);
  ASSERT_EQ(5,*x);
}
