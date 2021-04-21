#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/engine/test.h"

using namespace tableau;

TEST(cont_div,interface) {
  [[maybe_unused]] auto _ = engine::test::cont<engine::test::Div>;
}
