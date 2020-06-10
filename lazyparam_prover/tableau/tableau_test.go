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
  ppb "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/prover_go_proto"
)

func TestTransformations(t *testing.T) {
  ctx := context.Background()
  for _,trans := range []ppb.Transformation{
    ppb.Transformation_AXIOMATIC_EQ,
    ppb.Transformation_AXIOMATIC_EQ_FLAT,
    ppb.Transformation_AXIOMATIC_EQ_RESTRICTED_TRANS,
    ppb.Transformation_LP_MODIFICATION,
  } {
    for k,v := range problems.SampleProblems {
      k,v := k,v
      t.Run(fmt.Sprintf("case (%v,%q)",trans,k),func(t *testing.T) {
        out,err := Prove(ctx,v,ppb.Method_UNKNOWN_METHOD,trans,true)
        if err!=nil { t.Fatalf("Tableau(%q): %v",k,err) }
        tptpTransformed,err := tool.ProtoToTptp(ctx,out.TransformedProblem)
        if err!=nil { t.Fatalf("tool.ProtoToTptp(%q): %v",k,err) }
        t.Logf("out =\n%v",string(tptpTransformed))

        proveCtx,cancel := context.WithTimeout(ctx,10*time.Second)
        defer cancel()
        eproverOut,err := eprover.Prove(proveCtx,tptpTransformed)
        if err!=nil { t.Fatalf("eprover.Prove(%q): %v",k,err) }
        if !eproverOut.Solved { t.Fatalf("eprover.Prove(%q) = not solved",k) }
      })
    }
  }
}

func TestTableau(t *testing.T) {
  ctx := context.Background()
  for _,p := range []struct{
    method ppb.Method
    trans ppb.Transformation
  }{
    {ppb.Method_CONNECTION_TABLEAU, ppb.Transformation_AXIOMATIC_EQ},
    {ppb.Method_CONNECTION_TABLEAU, ppb.Transformation_AXIOMATIC_EQ_FLAT},
    {ppb.Method_CONNECTION_TABLEAU, ppb.Transformation_AXIOMATIC_EQ_RESTRICTED_TRANS},
    {ppb.Method_CONNECTION_TABLEAU, ppb.Transformation_LP_MODIFICATION},
    {ppb.Method_LAZY_PARAMODULATION, ppb.Transformation_SKIP},
  }{
    for k,v := range problems.SampleProblems {
      k,v := k,v
      t.Run(fmt.Sprintf("case (%v,%q)",p,k),func(t *testing.T) {
        proveCtx,cancel := context.WithTimeout(ctx,10*time.Second)
        defer cancel()
        out,err := Prove(proveCtx,v,p.method,p.trans,false)
        if err!=nil { t.Fatalf("Prove(%q): %v",k,err) }
        if out.Proof==nil {
          t.Fatalf("out = %+v",out)
        }

        proofTptp,err := tool.ProofToTptp(ctx,out.Proof)
        if err!=nil { t.Fatalf("tool.ProofTPTP(%q): %v",k,err) }
        t.Logf("proof = %s",proofTptp)

        if err!=nil { t.Fatalf("Tableau(%q): %v",k,err) }
        _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:out.CnfProblem,Proof:out.Proof})
        if err!=nil { t.Fatalf("tool.Validate(%q): %v",k,err) }
      })
    }
  }
}

