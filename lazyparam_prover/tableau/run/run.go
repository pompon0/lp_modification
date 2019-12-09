package main

import (
  "context"
  "log"
  "fmt"
  "flag"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

var caseName = flag.String("case_name","","")

func run(ctx context.Context) error {
  ps,cancel,err := problems.MizarProblems()
  if err!=nil { return fmt.Errorf("problems.MizarProblems(): %v",err) }
  defer cancel()
  p,ok := ps[*caseName]
  if !ok { return fmt.Errorf("case %q not found",*caseName) }
  tptp,err := p.Get()
  if err!=nil { return fmt.Errorf("p.Get(): %v",err) }
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
  if err!=nil { return fmt.Errorf("tool.TptpToProto(%q): %v",*caseName,err) }
  cnf,err := tool.FOFToCNF(ctx,fof)
  tptpCnfProblem,err := tool.ProtoToTptp(ctx,cnf)
  if err!=nil { return fmt.Errorf("tool.ProtoToTptp(): %v",err) }
  fmt.Printf("-- CNF BEGIN --\n%s-- CNF END--\n",tptpCnfProblem)

  if err!=nil { return fmt.Errorf("tool.FOFToCNF(%q): %v",*caseName,err) }
  out,err := tableau.Tableau(ctx,cnf,true)
  if err!=nil { return fmt.Errorf("Tableau(%q): %v",*caseName,err) }
  _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:cnf,Proof:out.Proof})
  if err!=nil { return fmt.Errorf("tool.Validate(%q): %v",*caseName,err) }

  tptpProof,err := tool.ProtoToTptp(ctx,out.Proof)
  if err!=nil { return fmt.Errorf("tool.ProtoToTptp(): %v",err) }
  fmt.Printf("-- PROOF BEGIN --\n%s-- PROOF END--\n",tptpProof)

  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
