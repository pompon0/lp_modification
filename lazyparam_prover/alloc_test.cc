#define DEBUG if(1)
#include "gtest/gtest.h"
#include "lazyparam_prover/alloc.h"
#include "lazyparam_prover/util/log.h"

TEST(RewindArray,simple) {
  StreamLogger _(std::cerr);
  RewindArray<int> a;
  a.resize(3);
  a.set(1,5);
  auto s = a.snapshot();
  a.resize(5);
  a.set(4,7);
  a.set(2,6);
  ASSERT_EQ(a.size(),5);
  ASSERT_EQ(a[0],Maybe<int>());
  ASSERT_EQ(a[1],Maybe<int>(5));
  ASSERT_EQ(a[2],Maybe<int>(6));
  ASSERT_EQ(a[3],Maybe<int>());
  ASSERT_EQ(a[4],Maybe<int>(7));
  a.rewind(s);
  ASSERT_EQ(a.size(),3);
  ASSERT_EQ(a[0],Maybe<int>());
  ASSERT_EQ(a[1],Maybe<int>(5));
  ASSERT_EQ(a[2],Maybe<int>());
}
