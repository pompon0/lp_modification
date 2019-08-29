package main

import (
  "fmt"
  "log"
  "time"
  "sort"
  "flag"
  "context"

  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
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
  prob,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("getProblems(): %v",err) }
  probCount := (len(prob)+mod-1)/mod

  probChan := make(chan Problem,16)
  resultsChan := make(chan Result,16)
  group,gCtx := errgroup.WithContext(ctx)

  var probNames []string
  for name,_ := range prob { probNames = append(probNames,name) }
  sort.Strings(probNames)

  group.Go(func() error {
    for i,name := range probNames {
      if *unsolvedOnly {
        sols,err := problems.ReadProofs(*proofDir,name)
        if err!=nil { return fmt.Errorf("readProofs(%q): %v",name,err) }
        if len(sols)>0 { continue }
      }
      if i%mod!=0 { continue }
      select {
      case <-gCtx.Done(): return nil
      case probChan <- Problem{name,prob[name]}:
      }
    }
    close(probChan)
    return nil
  })

  for i:=0; i<cores; i++ { group.Go(func() error {
    return worker(gCtx,timeout,tableau.Tableau,probChan,resultsChan)
  })}

  group.Go(func() error {
    okCount := 0
    newCount := 0
    errCount := 0
    for i:=0; i<probCount; i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r.err!=nil {
          log.Printf("cnfProblem[%q]: %v",r.name,r.err)
          errCount++
        } else {
          isNew,err := problems.WriteProof(*proofDir,r.name,r.solution)
          if err!=nil {
            return fmt.Errorf("writeProof(): %v",err)
          }
          if isNew { newCount++ }
          okCount++
        }
        log.Printf("done %v/%v err=%v ok=%v new=%v",i+1,probCount,errCount,okCount,newCount)
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

func main() {
  flag.Parse()
  if err := run(context.Background(),16*time.Second,4,1); err!=nil {
    log.Fatalf("%v",err)
  }
}
