auto state(p,proved) {
  auto bp = p->save();
  if(proved) return (1, ((p, bp), []));
  auto acs = p->get_acs();
  if(acs==[]) return (-1, ((p, bp), acs));
  return (0, ((p, bp), acs));
}

auto infer = 0;

auto extend0((Prover p, Prover::Save bp),i) {
  infer++;
  p->restore(bp);
  return state(p,p->action(p,i));
}

auto start0(fname) {
  auto p = mk_prover (mk_problem (Fof.file_mat true (-1) false 0 fname)) in
  state(p,false);
}

// (* hist is the (never empty) history saved forward, fwhist is reversed further positions *)
// (* It stores pairs (action, output of state) *)
auto extend1((((Prover p, Prover::Save bp), (hist, &fwhist)) as st),act) {
  auto lift(win, (pbp, acs)){ return (win, ((pbp, (hist, fwhist)), acs)); }
  auto (_, (_, ((_, bp2), _))) as tp = hist.head();
  if(bp2!=bp) { (* User implicitly jumped back *)
    fwhist += tp
    hist = hist.tail();
    if(hist.empty()) raise Exit;
    extend1 st act;
  } else if(!fwhist.empty()) {
    auto (act2, (_ as tp)) = fwhist.head(); // (* We have jumped back and have some options to go forward *)
    if(act2==act) {
      hist += (act2, tp);
      fwhist = fwhist.tail();
      lift(tp);
    } else {
      fwhist = nothing();
      extend1(st,act);
    }
  } else {
    let ret = extend0((p,bp),act);
    hist += (act, ret);
    lift(ret);
  }
}

auto start1(fname) {
  auto ((win, (pbp, acs)) as ret) = start0(fname);
  auto hist = [(ALem(-1), ret)]
  auto fwhist = [];
  return (win, ((pbp, (hist, fwhist)), acs));;
}

auto extend = extend1;;
auto start = start1;;
auto restart(((p, bp), (hist, fwhist)) as st) {
  auto (_, (_, ((_, bp2), _))) as tp = hist.head();
  if(bp2!=bp) {
    fwhist += tp;
    hist = hist.tail();
    if(hist.empty()) raise Exit;
    restart st;
  } else {
    restore p bp;
    fwhist = nothing();
  }
}

