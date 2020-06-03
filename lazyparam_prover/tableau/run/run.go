package main

import (
  "context"
  "log"
  "fmt"
  "flag"
  "sort"
  "time"
  "os"
  "io/ioutil"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

var all = flag.Bool("all",false,"")
var caseName = flag.String("case_name","","")
var timeout = flag.Duration("timeout",time.Hour,"")

func prove(ctx context.Context, tptp []byte) error {
  //fmt.Printf("%s",string(tptp))
  ctxProve,cancel := context.WithTimeout(ctx,*timeout)
  defer cancel()
  out,err := tableau.ProveLazyParamodulation(ctxProve,tptp)
  if err!=nil { return fmt.Errorf("Tableau(%q): %v",*caseName,err) }
  if !out.Solved {
    log.Printf("not solved")
    out.CnfProblem = nil
    log.Printf("out = %+v",out)
    return nil
  }
  tptpProblem,err := tool.ProtoToTptp(ctx,out.CnfProblem)
  if err!=nil { return fmt.Errorf("tool.ProtoToTptp(problem): %v",err) }
  fmt.Printf("-- PROBLEM BEGIN --\n%s-- PROBLEM END--\n",tptpProblem)
  tptpProof,err := tool.ProofToTptp(ctx,out.Proof)
  if err!=nil { return fmt.Errorf("tool.ProtoToTptp(%q): %v",*caseName,err) }
  fmt.Printf("-- PROOF BEGIN --\n%s-- PROOF END--\n",tptpProof)
  //log.Printf("out = %v",out)
  _,err = tool.ValidateProof(ctx,&spb.CNF{Problem:out.CnfProblem,Proof:out.Proof})
  if err!=nil { return fmt.Errorf("tool.Validate(%q): %v",*caseName,err) }
  return nil
}

func find() ([]byte,error) {
  mp,cancel,err := problems.MizarProblems()
  if err!=nil { return nil,fmt.Errorf("problems.MizarProblems(): %v",err) }
  defer cancel()
  tp,cancel,err := problems.TptpProblems()
  if err!=nil { return nil,fmt.Errorf("problems.TptpProblems(): %v",err) }
  defer cancel()
  var tptp []byte
  if *caseName!="" {
    p,ok := mp[*caseName]
    if !ok {
      p,ok = tp[*caseName]
    }
    if !ok { return nil,fmt.Errorf("case %q not found",*caseName) }
    if tptp,err = p.Get(); err!=nil {
      return nil,fmt.Errorf("p.Get(): %v",err)
    }
  } else {
    if tptp,err = ioutil.ReadAll(os.Stdin); err!=nil {
      return nil,fmt.Errorf("ioutil.ReadAll(): %v",err)
    }
  }
  return tptp,nil
}

type prob struct {
  name string
  tptp []byte
}

func run(ctx context.Context) error {
  if !*all {
    tptp,err := find()
    if err!=nil {
      return fmt.Errorf("find(%q): %w",*caseName,err)
    }
    return prove(ctx,tptp)
  } else {
    mp,cancel,err := problems.MizarProblems()
    if err!=nil { return fmt.Errorf("problems.MizarProblems(): %v",err) }
    defer cancel()
    var ps []prob
    for n,p := range mp {
      tptp,err := p.Get()
      if err!=nil { return fmt.Errorf("mizar[%q].Get(): %w",n,err) }
      ps = append(ps,prob{n,tptp})
    }
    sort.Slice(ps,func(i,j int) bool { return len(ps[i].tptp)<len(ps[j].tptp) })
    for _,p := range ps {
      log.Printf("== %q ==",p.name)
      if err:=prove(ctx,p.tptp); err!=nil {
        return fmt.Errorf("prove(%q): %w",p.name,err)
      }
    }
  }
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
