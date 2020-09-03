#define DEBUG_MODE
#include "utils/log.h"
#include "lazyparam_prover/memory/list.h"
#include "gtest/gtest.h"
using namespace tableau;

TEST(list,simple) {
  memory::Alloc A;
  List<int> x = nothing();
  x.push(A,5);
  ASSERT_EQ(5,x.head());
  ASSERT_EQ(1,x.size());
}
