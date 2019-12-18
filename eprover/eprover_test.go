package eprover

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

func TestProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range problems.SampleProblems {
    if out,err := Prove(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("Prover(%q): %v",name,err)
    }
  }
}

func TestFOFToCNF(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range problems.SampleProblems {
    tptpCNF,err := FOFToCNF(ctx,tptp)
    if err!=nil {
      t.Fatalf("Prover(%q): %v",name,err)
    }
    if _,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF); err!=nil {
      t.Fatalf("TptpToProto(%q): %v",name,err)
    }
  }
}
