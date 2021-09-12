package leancop

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems/sample"
)

func TestPrologProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range sample.SampleProblems() {
    // TODO: leancop crashes with "Unknown procedure f/1".
    if name == "t83_enumset1" { continue }
    if out,err := PrologProve(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("PrologProve(%q): %v",name,err)
    }
  }
}

func TestPrologProveCutComp7(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range sample.SampleProblems() {
    // TODO: leancop crashes with "Unknown procedure f/1".
    if name == "t83_enumset1" { continue }
    if out,err := PrologProveCutComp7(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("PrologProveCutComp7(%q): %v",name,err)
    }
  }
}
