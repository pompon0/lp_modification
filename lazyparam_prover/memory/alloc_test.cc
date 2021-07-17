#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/memory/array.h"
#include "lazyparam_prover/memory/maybe.h"
#include "utils/log.h"
#include "lazyparam_prover/memory/stack.h"

using namespace memory;

TEST(RewindArray,simple) {
  StreamLogger _(std::cerr);
  RewindArray<int> a;
  a.resize(3);
  a.set(1,5);
  auto s = a.save();
  a.resize(5);
  a.set(4,7);
  a.set(2,6);
  ASSERT_EQ(a.size(),5);
  ASSERT_EQ(a[0],Maybe<int>());
  ASSERT_EQ(a[1],Maybe<int>(5));
  ASSERT_EQ(a[2],Maybe<int>(6));
  ASSERT_EQ(a[3],Maybe<int>());
  ASSERT_EQ(a[4],Maybe<int>(7));
  a.restore(s);
  ASSERT_EQ(a.size(),3);
  ASSERT_EQ(a[0],Maybe<int>());
  ASSERT_EQ(a[1],Maybe<int>(5));
  ASSERT_EQ(a[2],Maybe<int>());
}

TEST(Array,copy) {
  memory::Alloc A;
  StreamLogger _(std::cerr);
  Array<int> a(A,3);
  a[1] = 7;
  auto b = a.copy(A);
  b[1] = 9;
  ASSERT_EQ(a[1],7);
  ASSERT_EQ(b[1],9);
}
