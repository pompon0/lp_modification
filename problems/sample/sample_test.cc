#define DEBUG_MODE
#include "gtest/gtest.h"
#include "utils/log.h"
#include "problems/sample/sample.h"

TEST(SAMPLE,simple) {
  StreamLogger _(std::cerr);
  problems::sample::sample_problems();
}
