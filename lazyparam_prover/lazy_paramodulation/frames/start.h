struct _StartFrame {
  template<typename Div> INL void run(Div *d) const {
    STATE_FRAME(d->A,d->state,"start");
    //TODO: next_starting_clause() has a side effect on state
    // make it more explicit.
    auto dcla = d->state->cla_index.next_starting_clause();
    if(!dcla) return;
    typename Div::Features f;
    f.set_depth([&]{ return 0; });
    d->or_(f,[dcla](Div *d)INLL{ strong(d,Branch(),dcla.get(),-1); });
    d->or_(f,[](Div *d)INLL{ _StartFrame{}.run(d); });
  }
};
