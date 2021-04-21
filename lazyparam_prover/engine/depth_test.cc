#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/engine/test.h"
#include "lazyparam_prover/engine/depth.h"

using namespace tableau;

TEST(depth,interface) {
  [[maybe_unused]] auto _ = engine::test::cont<engine::depth::Div>;
}
