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

template<typename Cont> Maybe<Cont> search(typename Cont::State &state, Cont c) {
  SCOPE("alt::search");
  struct Alt { Cont cont; typename Cont::State::Snapshot snapshot; };
  List<Alt> alts({c,state.snapshot()});
  for(size_t steps = 0; !alts.empty(); steps++) {
    DEBUG if(steps%1000==0) info("steps = %",steps);
    auto a = alts.head(); alts = alts.tail();
    state.rewind(a.snapshot);
    if(a.cont.done()) return Maybe<Cont>(a.cont);
    a.cont.run(state,
      [&alts,&state](Cont cont){ alts = Alt{cont,state.snapshot()} + alts; }
    );
  }
  return Maybe<Cont>();
}

} // namespace alt

#endif  // LAZYPARAM_PROVER_ALT_H_
