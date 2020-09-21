struct _StartFrame { size_t nodes_limit; };
using StartFrame = memory::Variant<Frame,Frame::START,_StartFrame>;

INL memory::List<Cont> start(memory::Alloc &A, StartFrame f) const { FRAME("start");
  memory::List<Cont> alts;
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
