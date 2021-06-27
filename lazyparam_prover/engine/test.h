#ifndef ENGINE_TEST_H_
#define ENGINE_TEST_H_

#include "lazyparam_prover/search_state.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/memory/function.h"
#include "lazyparam_prover/memory/maybe.h"

namespace tableau::engine::test {

struct Div {
  size_t size_limit(){ return 10; }
  memory::Alloc &A;
  SearchState *state;

  struct Alt {
    void feature_branch(Branch){}
    void feature_mcts_node(bool){}

    // argument of task will be executed asynchronously.
    template<typename F> INL void task(memory::Maybe<Atom> goal, F f) {
      static_assert(memory::has_sig<F,void(Div*)>());
    }
  };
  // alt executes argument synchronously.
  template<typename F> INL void alt(F f) {
    static_assert(memory::has_sig<F,void(Div::Alt*)>());
    // TODO: f should NOT modify the state (but it can allocate stuff)
    // Test it either by
    // - adding a lock on state somehow.
    // - full state comparison.
    Alt alt;
    f(&alt);
  }
};

template<typename Div> static void cont(Div *d){
  static_assert(memory::same<SearchState*,decltype(d->state)>::value);
  static_assert(memory::same<memory::Alloc&,decltype(d->A)>::value);
  auto s = [&]{ return d->size_limit(); };
  static_assert(memory::has_sig<decltype(s),size_t()>());
  d->alt([&](typename Div::Alt *x){
    x->feature_branch(Branch());
    x->feature_mcts_node(false);
    x->task(memory::just(Atom(d->A,true,4,{})),[](Div *d){});
  });
}

}  // namespace tableau::engine::test

#endif  // ENGINE_TEST_H_
