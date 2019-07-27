#ifndef LAZYPARAM_PROVER_ALT_H_
#define LAZYPARAM_PROVER_ALT_H_

namespace alt {

struct ExampleCont {
  struct State {
    struct Snapshot {};
    void rewind(Snapshot){}
    Snapshot snapshot(){ return {}; }
  };
  bool done() const { return 1; }
  template<typename Alts> void run(State &state, Alts &alts) const {}
};

template<typename Cont> bool search(typename Cont::State &state, Cont c) {
  SCOPE("alt::search");
  struct Alt { Cont cont; typename Cont::State::Snapshot snapshot; };
  List<Alt> stack({c,state.snapshot()});
  struct AltStack {
    typename Cont::State &state;
    List<Alt> &stack;
    inline void operator()(Cont cont){ stack += Alt{cont,state.snapshot()}; }
  };
  for(size_t steps = 0; !stack.empty(); steps++) {
    DEBUG if(steps%1000==0) info("steps = %",steps);
    auto a = stack.head(); stack = stack.tail();
    state.rewind(a.snapshot);
    if(a.cont.done()) return 1;
    a.cont.run(state,AltStack{state,stack});
  }
  return 0;
}

} // namespace alt

#endif  // LAZYPARAM_PROVER_ALT_H_
