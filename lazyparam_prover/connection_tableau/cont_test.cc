#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/connection_tableau/cont.h"
#include "lazyparam_prover/engine/test.h"

using namespace tableau;

TEST(cont,interface) {
  [[maybe_unused]] auto _ = connection_tableau::Cont::start<engine::test::Div>;
}
