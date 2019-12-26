package tableau

import (
  "fmt"
  "time"
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
    k,v := k,v
    t.Run(fmt.Sprintf("case %q",k),func(t *testing.T) {
      tptpCNF,err := eprover.FOFToCNF(ctx,v)
      if err!=nil { t.Fatalf("eprover.FOFToCNF(): %v",err) }
      cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
      if err!=nil { t.Fatalf("tool.TptpToProto(): %v",err) }
      proveCtx,cancel := context.WithTimeout(ctx,10*time.Second)
      defer cancel()
      out,err := Prove(proveCtx,v)
      if err!=nil { t.Fatalf("Tableau(%q): %v",k,err) }
      _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:cnf,Proof:out.Proof})
      if err!=nil { t.Fatalf("tool.Validate(%q): %v",k,err) }
    })
  }
}

