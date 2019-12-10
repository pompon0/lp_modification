package problems

import (
  "context"
  "testing"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

func TestTptpProblems(t *testing.T) {
  ps,cancel,err := TptpProblems()
  if err!=nil { t.Fatalf("TptpProblems(): %v",err) }
  defer cancel()
  if want,got := tptpProblemsCount,len(ps); got!=want {
    t.Fatalf("len(ps) = %v, want %v)",got,want)
  }
  for n,p := range ps {
    tptp,err := p.Get()
    if err!=nil { t.Fatalf("ps[%q].Get(): %v",n,err) }
    if _,err := tool.TptpToProto(context.Background(),tool.FOF,tptp); err!=nil {
      t.Fatalf("tool.TptpToProto(%q): %v",n,err)
    }
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
