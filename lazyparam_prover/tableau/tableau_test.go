package provers

import (
  "testing"
  "context"
  "io/ioutil"

  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/utils"
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
    if !valid {
      t.Errorf("tool.Validate(%q) = %v",k,valid)
    }
  }
}


func TestProblematicTableau(t *testing.T) {
  ctx := context.Background()
  k := "tptp_sample/f/t25_yellow_1"
  tptp,err := ioutil.ReadFile(utils.Runfile(k))
  if err!=nil { t.Fatalf("ioutil.ReadFile(): %v",err) }
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
  if err!=nil { t.Fatalf("tool.TptpToProto(%q): %v",k,err) }
  cnf,err := tool.FOFToCNF(ctx,fof)
  if err!=nil { t.Fatalf("tool.FOFToCNF(%q): %v",k,err) }
  proof,err := Tableau(ctx,cnf)
  if err!=nil { t.Fatalf("Tableau(%q): %v",k,err) }
  _,err = tool.ValidateProof(ctx,cnf,proof)
  if err!=nil { t.Errorf("tool.Validate(%q): %v",k,err) }
}
