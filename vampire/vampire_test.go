package vampire

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems/sample"
)

func TestProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range sample.SampleProblems() {
    if out,err := Prove(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("Prover(%q): %v",name,err)
    }
  }
}

func TestProveNoEq(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range sample.SampleProblems() {
    hasEq,err := tptp.HasEquality(ctx)
    if err!=nil {
      t.Fatalf("tool.TptpHasEquality(%q): %v",name,err)
    }
    if hasEq { continue }
    if out,err := Prove(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("Prover(%q): %v",name,err)
    }
  }
}
