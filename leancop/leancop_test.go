package leancop

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems/sample"
)

func TestProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range sample.SampleProblems() {
    // leancop doesn't understand the trivial example 
    if name == "trivial" { continue }
    // cases not covered due to incompleteness
    if name == "l40_tex_2_reduced" { continue }
    if out,err := Prove(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("Prove(%q): %v,%v",name,out,err)
    }
  }
}
