package main

import (
  "fmt"
  "log"
  "time"
  "context"

  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  tpb "github.com/pompon0/tptp_parser/proto/tptp_go_proto"
)

type Prover func(ctx context.Context, cnfProblem *tpb.File) (cnfProof *tpb.File, err error)

type Problem struct {
  name string
  cnfProblem *tpb.File
}

type Result struct {
  name string
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
      if err := func() error {
        proverCtx,cancel := context.WithTimeout(ctx,timeout)
        defer cancel()
        err := func() error {
          cnfProof,err := prover(proverCtx,p.cnfProblem)
          if err!=nil { return err }
          if _,err := tool.ValidateProof(ctx,p.cnfProblem,cnfProof); err!=nil { return err }
          return nil
        }()
        results <- Result{p.name,err}
        return nil
      }(); err!=nil { return err }
    }
  }
}

func run(ctx context.Context, timeout time.Duration, cores int) error {
  problems,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("getProblems(): %v",err) }
  log.Printf("problems downloaded")

  count := 0
  cnfProblems := map[string]*tpb.File{}
  for name,tptp := range problems {
    count++
    if count==100 { break }
    fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
    if err!=nil { return fmt.Errorf("tool.TptpToProto(%q): %v",name,err) }
    cnf,err := tool.FOFToCNF(ctx,fof)
    if err!=nil { return fmt.Errorf("tool.FOFTOCNF(%q): %v",name,err) }
    cnfProblems[name] = cnf
  }
  log.Printf("problems converted")

  problemsChan := make(chan Problem,5)
  resultsChan := make(chan Result,5)
  group,gCtx := errgroup.WithContext(ctx)

  group.Go(func() error {
    for name,cnfProblem := range cnfProblems {
      select {
      case <-gCtx.Done(): return nil
      case problemsChan <- Problem{name,cnfProblem}:
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
    errCount := 0
    for i:=0; i<len(problems); i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r.err!=nil {
          log.Printf("cnfProblem[%q]: %v",r.name,r.err)
          errCount++
        } else {
          okCount++
        }
        log.Printf("done %v/%v err=%v ok=%v",i+1,len(cnfProblems),errCount,okCount)
      }
    }
    return nil
  })

  if err := group.Wait(); err!=nil {
    return fmt.Errorf("group.Wait(); %v",err)
  }

  return nil
}

func main() {
  if err := run(context.Background(),10*time.Second,4); err!=nil {
    log.Fatalf("%v",err)
  }
}
