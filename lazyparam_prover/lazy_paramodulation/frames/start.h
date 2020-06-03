struct _StartFrame { size_t nodes_limit; };
using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

template<typename Alts> void start(State &state, StartFrame f, Alts alts) const { STATE_FRAME(state,"start");
  while(auto dcla = state.cla_index.next_starting_clause()) {
    StrongFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branch = Branch();
    b->dcla = dcla.get();
    b->strong_id = -1;
    alts(Cont{List<Frame>(Frame(b.build()))});
  }
}
