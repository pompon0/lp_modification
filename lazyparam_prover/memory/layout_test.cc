#define DEBUG_MODE
#include "gtest/gtest.h"
#include "lazyparam_prover/memory/layout.h"
#include "lazyparam_prover/memory/stack.h"

using namespace memory;

TEST(layout,simple) {
  StreamLogger _(std::cerr);
  struct T { int x,y; };
  using A = Field<int>;
  using B = Field<T,A>;
  using C = ArrayField<int,B>;
  Alloc M;
  auto c = C::alloc(M,7);
  c.A::ref() = 1;
  c.B::ref().x = 2;
  c.C::ref(3) = 8;
  B b = c;
  b.A::ref() = 9;
  using X = Variant<0,int>;
  using Y = Variant<1,float>;
  using Z = Variant<2,double>;
  using Any = Coprod<X,Y,Z>;
  auto x = X::alloc(M);
  x.ref() = 5;
  auto z = X(Any(x));
}
