#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/lazy_paramodulation/cont.h"
#include "lazyparam_prover/engine/test.h"

using namespace tableau;

TEST(cont,interface) {
  [[maybe_unused]] auto _ = lazy_paramodulation::Cont::start<engine::test::Div>;
}
