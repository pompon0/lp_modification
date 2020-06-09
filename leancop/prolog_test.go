package leancop

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems"
)

func TestPrologProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range problems.SampleProblems {
    if out,err := PrologProve(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("PrologProve(%q): %v",name,err)
    }
  }
}

func TestPrologProveCutComp7(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range problems.SampleProblems {
    if out,err := PrologProveCutComp7(ctx,tptp); err!=nil || !out.Solved {
      t.Fatalf("PrologProveCutComp7(%q): %v",name,err)
    }
  }
}
