package main

import (
  "context"
  "log"
  "fmt"
  "flag"
  "time"
  "os"
  "io/ioutil"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

var caseName = flag.String("case_name","","")
var timeout = flag.Duration("timeout",time.Hour,"")

func run(ctx context.Context) error {
  mp,cancel,err := problems.MizarProblems()
  if err!=nil { return fmt.Errorf("problems.MizarProblems(): %v",err) }
  defer cancel()
  tp,cancel,err := problems.TptpProblems()
  if err!=nil { return fmt.Errorf("problems.TptpProblems(): %v",err) }
  defer cancel()
  var tptp []byte
  if *caseName!="" {
    p,ok := mp[*caseName]
    if !ok {
      p,ok = tp[*caseName]
    }
    if !ok { return fmt.Errorf("case %q not found",*caseName) }
    if tptp,err = p.Get(); err!=nil {
      return fmt.Errorf("p.Get(): %v",err)
    }
  } else {
    if tptp,err = ioutil.ReadAll(os.Stdin); err!=nil {
      return fmt.Errorf("ioutil.ReadAll(): %v",err)
    }
  }
  //fmt.Printf("%s",string(tptp))
  ctxProve,cancel := context.WithTimeout(ctx,*timeout)
  defer cancel()
  out,err := tableau.Prove(ctxProve,tptp)
  if err!=nil { return fmt.Errorf("Tableau(%q): %v",*caseName,err) }
  if !out.Solved {
    log.Printf("not solved")
    log.Printf("out = %+v",out)
    return nil
  }
  log.Printf("out = %v",out)
  _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:out.CnfProblem,Proof:out.Proof})
  if err!=nil { return fmt.Errorf("tool.Validate(%q): %v",*caseName,err) }
  tptpProof,err := tool.ProtoToTptp(ctx,out.Proof)
  if err!=nil { return fmt.Errorf("tool.ProtoToTptp(%q): %v",*caseName,err) }
  fmt.Printf("-- PROOF BEGIN --\n%s-- PROOF END--\n",tptpProof)

  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
