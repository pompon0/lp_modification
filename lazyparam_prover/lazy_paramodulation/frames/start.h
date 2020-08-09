struct _StartFrame { size_t nodes_limit; };
using StartFrame = Variant<Frame,Frame::START,_StartFrame>;

List<Cont> start(memory::Alloc &A, StartFrame f) const { STATE_FRAME(A,state,"start");
  List<Cont> alts;
  while(auto dcla = state->cla_index.next_starting_clause()) {
    StrongFrame::Builder b(A);
    b->nodes_limit = f->nodes_limit;
    b->branch = Branch();
    b->dcla = dcla.get();
    b->strong_id = -1;
    alts.push(A,builder().add(A,Frame(b.build())).build());
  }
  return alts;
}
