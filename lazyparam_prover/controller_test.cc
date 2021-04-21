#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/controller.h"
#include "lazyparam_prover/engine/test.h"

TEST(CONTROLLER,interface) {
  StreamLogger _l(std::cerr);
  [[maybe_unused]] auto _ = tableau::engine::test::cont<controller::Div>;  
}
