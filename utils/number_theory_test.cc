#include "gtest/gtest.h"
#include "utils/types.h"
#include "utils/number_theory.h"

using namespace util;

TEST(inv,simple) {
  for(auto x : vec<uint64_t>{1,15,78934857,8745032764389431}) {
    EXPECT_EQ(1,uint8_t(x*inv<uint8_t>(x)));
    EXPECT_EQ(1,uint16_t(x*inv<uint16_t>(x)));
    EXPECT_EQ(1,uint32_t(x*inv<uint32_t>(x)));
    EXPECT_EQ(1,uint64_t(x*inv<uint64_t>(x)));
  }
}
