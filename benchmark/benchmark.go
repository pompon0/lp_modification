package main

import (
  "fmt"
  "log"
  "path"
  "path/filepath"
  "time"
  "sort"
  "flag"
  "context"
  "io/ioutil"
  "os"

  "golang.org/x/sync/errgroup"
  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  tpb "github.com/pompon0/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_parser/proto/solutions_go_proto"
)

type Prover func(ctx context.Context, cnfProblem *tpb.File, streamStdErr bool) (cnfProof *tpb.File, err error)

type Problem struct {
  name string
  tptpFOFProblem []byte
}

type Result struct {
  name string
  solution *spb.CNF
  err error
}

func worker(
  ctx context.Context,
  timeout time.Duration,
  prover Prover,
  problems <-chan Problem,
  results chan<- Result,
) error {
  for {
    select {
    case <-ctx.Done(): return nil
    case p,ok := <-problems:
      if !ok { return nil }
      cnfProblem,err := convProblem(ctx,p.tptpFOFProblem)
      if err!=nil { return fmt.Errorf("convProblem(%q): %v",p.name,err) }
      if err := func() error {
        proverCtx,cancel := context.WithTimeout(ctx,timeout)
        defer cancel()
        proof, err := prover(proverCtx,cnfProblem,false)
        if err ==nil {
          if _,err := tool.ValidateProof(ctx,cnfProblem,proof); err!=nil { return err }
        }
        results <- Result{p.name,&spb.CNF{Problem:cnfProblem,Proof:proof},err}
        return nil
      }(); err!=nil { return err }
    }
  }
}

func convProblem(ctx context.Context, tptp []byte) (*tpb.File,error) {
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
  if err!=nil { return nil,fmt.Errorf("tool.TptpToProto(): %v",err) }
  cnf,err := tool.FOFToCNF(ctx,fof)
  if err!=nil { return nil,fmt.Errorf("tool.FOFTOCNF(): %v",err) }
  return cnf,nil
}

func run(ctx context.Context, timeout time.Duration, cores int, mod int) error {
  problems,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("getProblems(): %v",err) }
  problemsCount := (len(problems)+mod-1)/mod

  problemsChan := make(chan Problem,16)
  resultsChan := make(chan Result,16)
  group,gCtx := errgroup.WithContext(ctx)

  var problemNames []string
  for name,_ := range problems { problemNames = append(problemNames,name) }
  sort.Strings(problemNames)

  group.Go(func() error {
    for i,name := range problemNames {
      if *unsolvedOnly {
        sols,err := readProofs(name)
        if err!=nil { return fmt.Errorf("readProofs(%q): %v",name,err) }
        if len(sols)>0 { continue }
      }
      if i%mod!=0 { continue }
      select {
      case <-gCtx.Done(): return nil
      case problemsChan <- Problem{name,problems[name]}:
      }
    }
    close(problemsChan)
    return nil
  })

  for i:=0; i<cores; i++ { group.Go(func() error {
    return worker(gCtx,timeout,tableau.Tableau,problemsChan,resultsChan)
  })}

  group.Go(func() error {
    okCount := 0
    newCount := 0
    errCount := 0
    for i:=0; i<problemsCount; i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r.err!=nil {
          log.Printf("cnfProblem[%q]: %v",r.name,r.err)
          errCount++
        } else {
          isNew,err := writeProof(r.name,r.solution)
          if err!=nil {
            return fmt.Errorf("writeProof(): %v",err)
          }
          if isNew { newCount++ }
          okCount++
        }
        log.Printf("done %v/%v err=%v ok=%v new=%v",i+1,problemsCount,errCount,okCount,newCount)
      }
    }
    return nil
  })

  if err := group.Wait(); err!=nil {
    return fmt.Errorf("group.Wait(); %v",err)
  }

  return nil
}

var proofDir = flag.String("proof_dir","/tmp/benchmark_proofs","output directory to write proofs to")
var unsolvedOnly = flag.Bool("unsolved_only",false,"process only unsolved problems")

func readProofs(name string) ([]*spb.CNF,error) {
  p := path.Join(*proofDir,name)
  var solutions []*spb.CNF
  if _,err := os.Stat(p); os.IsNotExist(err) { return nil,nil }
  if err := filepath.Walk(p,func(fp string, fi os.FileInfo, err error) error {
    if err!=nil { return err }
    if fi.IsDir() { return nil }
    raw,err := ioutil.ReadFile(fp)
    if err!=nil { return fmt.Errorf("ioutil.ReadAll(): %v",err) }
    solution := &spb.CNF{}
    if err:=proto.UnmarshalText(string(raw),solution); err!=nil {
      return fmt.Errorf("proto.UnmarshalText(): %v",err)
    }
    solutions = append(solutions,solution)
    return nil
  }); err!=nil { return nil,fmt.Errorf("filepath.Walk(): %v",err) }
  return solutions,nil
}

func writeProof(name string, solution *spb.CNF) (bool,error) {
  p := path.Join(*proofDir,name)
  if _,err := os.Stat(p); os.IsNotExist(err) {
    if err := os.MkdirAll(p,0777); err!=nil {
      return false,fmt.Errorf("os.MkDirAll(): %v",err)
    }
  }
  oldSolutions,err := readProofs(name)
  if err!=nil{ return false,fmt.Errorf("readProofs(%q): %v",name,err) }
  found := false
  for _,s := range oldSolutions {
    found = found || proto.Equal(solution,s)
  }
  if !found {
    err := ioutil.WriteFile(path.Join(p,time.Now().Format("2006-01-02_15:04:05")),[]byte(solution.String()),0666)
    if err!=nil { return false,fmt.Errorf("ioutil.WriteFile(): %v",err) }
  }
  return len(oldSolutions)==0,nil
}

func main() {
  flag.Parse()
  if err := run(context.Background(),16*time.Second,4,1); err!=nil {
    log.Fatalf("%v",err)
  }
}
