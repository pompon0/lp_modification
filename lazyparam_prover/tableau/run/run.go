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
  "strings"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/problems/sample"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
  ppb "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/prover_go_proto"
)

var all = flag.Bool("all",false,"")
var caseName = flag.String("case_name","","")
var timeout = flag.Duration("timeout",time.Hour,"")

var trans = (*ppb.Transformation)(utils.NewEnumFlag("trans", ppb.Transformation_LP_MODIFICATION))
var transOnly = flag.Bool("trans_only",false,"")
var method = (*ppb.Method)(utils.NewEnumFlag("method", ppb.Method_CONNECTION_TABLEAU))

var funOrdPath = flag.String("fun_ord_path","","Path to a file with FunOrd text proto.")

func tptpInputString(i *tpb.Input) string {
  return fmt.Sprintf("%v",i.Formula)
}

func tptpFileString(f *tpb.File) string {
  b := &strings.Builder{}
  for ii,i := range f.Input {
    fmt.Fprintf(b,"Input[%d] = %v\n",ii,tptpInputString(i))
  }
  for _,n := range f.Nodes {
    fmt.Fprintf(b,"Nodes = %v\n",n)
  }
  return b.String()
}

func tptpProofString(p *spb.Proof) string {
  b := &strings.Builder{}
  for ci,c := range p.Clauses {
    for si,s := range c.Sources {
      fmt.Fprintf(b,"Clauses[%d].Sources[%d].ground = %v\n",ci,si,tptpInputString(s.Ground))
      fmt.Fprintf(b,"Clauses[%d].Sources[%d].source = %v\n",ci,si,tptpInputString(s.Source))
    }
  }
  for _,n := range p.Nodes {
    fmt.Fprintf(b,"Nodes = %v\n",n)
  }
  return b.String()
}

func prove(ctx context.Context, tptp []byte, funOrd *spb.FunOrd) error {
  ctxProve,cancel := context.WithTimeout(ctx,*timeout)
  defer cancel()
  out,err := tableau.Prove(ctxProve,tptp,funOrd,*method,*trans,*transOnly)
  if err!=nil { return fmt.Errorf("Tableau(%q): %v",*caseName,err) }
  if !out.Solved {
    log.Printf("not solved")
    out.CnfProblem = nil
    if *transOnly {
      log.Printf("out.TransformedProblem = %s",tptpFileString(out.TransformedProblem))
      tptpProblem,err := tool.ProtoToTptp(ctx,out.TransformedProblem)
      if err!=nil { return fmt.Errorf("tool.ProtoToTptp(out.TransformedProblem): %v",err) }
      fmt.Printf("-- PROBLEM BEGIN --\n%s-- PROBLEM END--\n",tptpProblem)
    } else {
      log.Printf("out = %+v",out)
    }
    return nil
  }
  log.Printf("out.CnfProblem = %v",out.CnfProblem)
  log.Printf("out.Proof = %s",tptpProofString(out.Proof))
  tptpProblem,err := tool.ProtoToTptp(ctx,out.CnfProblem)
  if err!=nil { return fmt.Errorf("tool.ProtoToTptp(problem): %v",err) }
  fmt.Printf("-- PROBLEM BEGIN --\n%s-- PROBLEM END--\n",tptpProblem)
  tptpProof,err := tool.ProofToTptp(ctx,out.Proof)
  if err!=nil { return fmt.Errorf("tool.ProofToTptp(%q): %v",*caseName,err) }
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
  sp := sample.SampleProblems()
  var tptp []byte
  if *caseName!="" {
    if p,ok := mp[*caseName]; ok {
      return p.Get()
    }
    if p,ok := tp[*caseName]; ok {
      return p.Get()
    }
    if p,ok := sp[*caseName]; ok {
      return p,nil
    }
    return nil,fmt.Errorf("case %q not found",*caseName)
  } else {
    if tptp,err = ioutil.ReadAll(os.Stdin); err!=nil {
      return nil,fmt.Errorf("ioutil.ReadAll(): %v",err)
    }
    return tptp,nil
  }
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
    var funOrd spb.FunOrd
    if *funOrdPath!="" {
      funOrdRaw,err := ioutil.ReadFile(*funOrdPath)
      if err!=nil { return fmt.Errorf("ioutil.ReadFile(): %w") }
      if err:=proto.UnmarshalText(string(funOrdRaw),&funOrd); err!=nil {
        return fmt.Errorf("proto.UnmarshalText(funOrd): %w",err)
      }
    }
    return prove(ctx,tptp,&funOrd)
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
      if err:=prove(ctx,p.tptp,nil); err!=nil {
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
