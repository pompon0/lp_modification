package main

import (
  "context"
  "log"
  "fmt"
  "io/ioutil"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/utils"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

func run(ctx context.Context) error {
  //k := "tptp_sample/f/t25_yellow_1"
  k := "tptp_sample/f/t32_funct_1"
  tptp,err := ioutil.ReadFile(utils.Runfile(k))
  if err!=nil { return fmt.Errorf("ioutil.ReadFile(): %v",err) }
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
  if err!=nil { return fmt.Errorf("tool.TptpToProto(%q): %v",k,err) }
  cnf,err := tool.FOFToCNF(ctx,fof)
  if err!=nil { return fmt.Errorf("tool.FOFToCNF(%q): %v",k,err) }
  proof,err := tableau.Tableau(ctx,cnf,true)
  if err!=nil { return fmt.Errorf("Tableau(%q): %v",k,err) }
  _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:cnf,Proof:proof})
  if err!=nil { return fmt.Errorf("tool.Validate(%q): %v",k,err) }
  return nil
}

func main() {
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
