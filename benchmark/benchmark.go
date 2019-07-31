package main

import (
  "fmt"
  "log"
  "time"
  "sort"
  "context"

  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  tpb "github.com/pompon0/tptp_parser/proto/tptp_go_proto"
)

type Prover func(ctx context.Context, cnfProblem *tpb.File, streamStdErr bool) (cnfProof *tpb.File, err error)

type Problem struct {
  name string
  tptpFOFProblem []byte
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
      cnfProblem,err := convProblem(ctx,p.tptpFOFProblem)
      if err!=nil { return fmt.Errorf("convProblem(%q): %v",p.name,err) }
      if err := func() error {
        proverCtx,cancel := context.WithTimeout(ctx,timeout)
        defer cancel()
        err := func() error {
          cnfProof,err := prover(proverCtx,cnfProblem,false)
          if err!=nil { return err }
          if _,err := tool.ValidateProof(ctx,cnfProblem,cnfProof); err!=nil { return err }
          return nil
        }()
        results <- Result{p.name,err}
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
  log.Printf("problems downloaded")
  problemsCount := (len(problems)+mod-1)/mod

  problemsChan := make(chan Problem,16)
  resultsChan := make(chan Result,16)
  group,gCtx := errgroup.WithContext(ctx)

  var problemNames []string
  for name,_ := range problems { problemNames = append(problemNames,name) }
  sort.Strings(problemNames)

  group.Go(func() error {
    for i,name := range problemNames {
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
    errCount := 0
    for i:=0; i<problemsCount; i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r.err!=nil {
          log.Printf("cnfProblem[%q]: %v",r.name,r.err)
          errCount++
        } else {
          okCount++
        }
        log.Printf("done %v/%v err=%v ok=%v",i+1,problemsCount,errCount,okCount)
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
  if err := run(context.Background(),4*time.Second,4,1); err!=nil {
    log.Fatalf("%v",err)
  }
}
