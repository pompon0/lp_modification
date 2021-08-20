package mcts

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems/sample"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

func TestProve(t *testing.T) {
  ctx := context.Background()
  for name,tptp := range sample.SampleProblems() {
    out,err := Prove(ctx,tptp)
    if err!=nil || !out.Solved {
      t.Fatalf("Prove(%q): %v",name,err)
    }
    if out.CnfProblem==nil || out.Proof==nil {
      t.Fatalf("out = %+v",out)
    }
    _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:out.CnfProblem,Proof:out.Proof})
    if err!=nil { t.Fatalf("tool.Validate(%q): %v",name,err) }
  }
}
