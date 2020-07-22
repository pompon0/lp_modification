struct State {
  int win; //-1,0,1
  Prover *p;
  Prover::Save pb;
  vec<Action> actions;
};

State state(Prover *p, bool proved) {
  auto bp = p->save();
  vec<Action> actions = proved ? {} : p->get_acs();
  return State {
    .win = proved ? 1 : actions.size() ? 0 : -1,
    .p = p,
    .bp = bp,
    .actions = actions,
  };
}

Start start(fname) {
  auto p = mk_prover(mk_problem (Fof.file_mat true (-1) false 0 fname));
  return state(p,false);
}

State restart(State st) {
  st.p->restore(st.bp);
  return st;
}

// (* hist is the (never empty) history saved forward, fwhist is reversed further positions *)
// (* It stores pairs (action, output of state) *)
StartRes extend(State st, Action act) {
  st.p->restore(st.bp);
  return state(st.p,st.p->action(act));
}
