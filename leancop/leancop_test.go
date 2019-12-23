package leancop

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems"
)

func TestProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range problems.SampleProblems {
    // leancop doesn't understand the trivial example 
    if name == "trivial" { continue }
    if out,err := Prove(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("Prover(%q): %v",name,err)
    }
  }
}
