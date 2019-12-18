package tableau

import (
  "testing"
  "context"

  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

func TestTableau(t *testing.T) {
  ctx := context.Background()
  for k,v := range problems.SampleProblems {
    t.Logf("case %q",k)
    tptpCNF,err := eprover.FOFToCNF(ctx,v)
    if err!=nil { t.Fatalf("eprover.FOFToCNF(): %v",err) }
    cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
    if err!=nil { t.Fatalf("tool.TptpToProto(): %v",err) }
    out,err := Prove(ctx,v)
    if err!=nil { t.Fatalf("Tableau(%q): %v",k,err) }
    _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:cnf,Proof:out.Proof})
    if err!=nil { t.Fatalf("tool.Validate(%q): %v",k,err) }
  }
}

