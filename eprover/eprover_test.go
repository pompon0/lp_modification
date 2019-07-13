package eprover

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems"
)

func TestProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range problems.SampleProblems {
    if err := Prove(ctx,tptp); err!=nil {
      t.Fatalf("Prover(%q): %v",name,err)
    }
  }
}
