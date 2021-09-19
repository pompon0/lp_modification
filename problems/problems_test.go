package problems

import (
  "log"
  "context"
  "testing"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/eprover"
)

func TestTptpProblems(t *testing.T) {
  ctx := context.Background()
  ps,_,err := TptpProblems()
  if err!=nil { t.Fatalf("TptpProblems(): %v",err) }
  if want,got := tptpProblemsCount,len(ps); got!=want {
    t.Fatalf("len(ps) = %v, want %v)",got,want)
  }
  for n,p := range ps {
    n,p := n,p
    t.Run(n, func(t *testing.T) {
      t.Parallel()
      tptp,err := p.Get()
      if err!=nil { t.Fatalf("ps[%q].Get(): %v",n,err) }
      defer log.Printf("%q DONE",n)
      cnf,err := eprover.FOFToCNF(ctx,tptp)
      if err!=nil { t.Fatalf("eprover.FOFToCNF(%q): %v",n,err) }
      _,err = cnf.ToProto(ctx,tool.CNF)
      if err!=nil { t.Fatalf("tool.TptpToProto(%q): %v",n,err) }
    })
  }
}

func TestMizarProblems(t *testing.T) {
  ps,cancel,err := MizarProblems()
  if err!=nil { t.Fatalf("MizarProblems(): %v",err) }
  defer cancel()
  if want,got := mizarProblemsCount,len(ps); got!=want {
    t.Fatalf("len(ps) = %v, want %v)",got,want)
  }
  for n,p := range ps {
    if _,err := p.Get(); err!=nil { t.Errorf("ps[%q].Get(): %v",n,err) }
  }
}
