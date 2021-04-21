#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/engine/test.h"
#include "lazyparam_prover/engine/balanced.h"

using namespace tableau;

TEST(balanced,interface) {
  [[maybe_unused]] auto _ = engine::test::cont<engine::balanced::Proxy>;
}
