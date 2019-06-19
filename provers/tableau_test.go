package provers

import (
  "testing"
  "context"

  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

func TestTableau(t *testing.T) {
  ctx := context.Background()
  for k,v := range problems.SampleProblems {
    fof,err := tool.TptpToProto(ctx,tool.FOF,v)
    if err!=nil { t.Fatalf("tool.TptpToProto(%q): %v",k,err) }
    cnf,err := tool.FOFToCNF(ctx,fof)
    if err!=nil { t.Fatalf("tool.FOFToCNF(%q): %v",k,err) }
    proof,err := Tableau(ctx,cnf)
    if err!=nil { t.Errorf("Tableau(%q): %v",k,err) }
    valid,err := tool.ValidateProof(ctx,cnf,proof)
    if err!=nil { t.Errorf("tool.Validate(%q): %v",k,err) }
    if !valid { t.Errorf("tool.Validate(%q) = %v",k,valid) }
  }
}
