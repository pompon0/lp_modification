struct _StartFrame { size_t nodes_limit; };
using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

List<Cont> start(StartFrame f) const { FRAME("start");
  List<Cont> alts;
  while(auto dcla = state->cla_index.next_starting_clause()) {
    StrongFrame::Builder b;
    b->nodes_limit = f->nodes_limit;
    b->branch = Branch();
    b->dcla = dcla.get();
    b->strong_id = -1;
    alts += cont(Frame(b.build()));
  }
  return alts;
}
