package tableau

import (
  "testing"
  "context"

  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

func TestTableau(t *testing.T) {
  ctx := context.Background()
  for k,v := range problems.SampleProblems {
    t.Logf("case %q",k)
    fof,err := tool.TptpToProto(ctx,tool.FOF,v)
    if err!=nil { t.Fatalf("tool.TptpToProto(%q): %v",k,err) }
    cnf,err := tool.FOFToCNF(ctx,fof)
    if err!=nil { t.Fatalf("tool.FOFToCNF(%q): %v",k,err) }
    out,err := Tableau(ctx,cnf,true,false)
    if err!=nil { t.Fatalf("Tableau(%q): %v",k,err) }
    _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:cnf,Proof:out.Proof})
    if err!=nil { t.Fatalf("tool.Validate(%q): %v",k,err) }
  }
}

