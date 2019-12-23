package main

import (
  "fmt"
  "log"
  "time"
  "context"
  "flag"
  "sort"
  "strings"

  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/vampire"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/leancop"
  "github.com/golang/protobuf/ptypes"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

type Prover func(ctx context.Context, fofProblem []byte) (*spb.ProverOutput,error)

var provers = []struct { name string; prover Prover } {
  {"tableau", tableau.Prove},
  {"vampire", vampire.Prove},
  {"eprover", eprover.Prove},
  {"leancop", leancop.Prove},
  {"leancop_prolog", leancop.PrologProve},
}

type Problem struct {
  *problems.Problem
  name string
}

type Result struct {
  name string
  proverResults map[string]*spb.Case
}

func worker(
  ctx context.Context,
  timeout time.Duration,
  problems <-chan Problem,
  results chan<- Result,
) error {
  for {
    select {
    case <-ctx.Done(): return nil
    case p,ok := <-problems:
      if !ok { return nil }
      tptpFOFProblem,err := p.Get()
      if err!=nil { return fmt.Errorf("p[%q].Get(): %v",p.name,err) }
      res := Result{p.name,map[string]*spb.Case{}}
      for _,prover := range provers {
        if err := func() error {
          proverCtx,cancel := context.WithTimeout(ctx,timeout)
          defer cancel()
          start := time.Now()
          out,err := prover.prover(proverCtx,tptpFOFProblem)
          if err!=nil { return fmt.Errorf("prover[%q].prover(%q): %v",prover.name,p.name,err) }
          res.proverResults[prover.name] = &spb.Case{
            Name: p.name,
            Duration: ptypes.DurationProto(time.Now().Sub(start)),
            Output: out,
          }
          return nil
        }(); err!=nil {
          return err
        }
      }
      results <- res
    }
  }
}

////////////////////////////////////////////

const (
  problemSetMizar = "mizar"
  problemSetTptp = "tptp"
)
var problemSets = []string{problemSetMizar,problemSetTptp}

var problemSet = flag.String("problem_set",problemSetMizar,strings.Join(problemSets,"|"))

func run(ctx context.Context, timeout time.Duration, cores int) error {
  var ps map[string]*problems.Problem
  switch *problemSet {
  case problemSetMizar:
    x,cancel,err := problems.MizarProblems()
    if err!=nil { return fmt.Errorf("problems.MizarProblems(): %v",err) }
    defer cancel()
    ps = x
  case problemSetTptp:
    x,cancel,err := problems.TptpProblems()
    if err!=nil { return fmt.Errorf("problems.TptpProblems(): %v",err) }
    defer cancel()
    ps = x
  default:
    return fmt.Errorf("unknown problem set %q",*problemSet)
  }

  problemsChan := make(chan Problem,16)
  resultsChan := make(chan Result,16)
  group,gCtx := errgroup.WithContext(ctx)

  var problemNames []string
  for name,_ := range ps { problemNames = append(problemNames,name) }
  sort.Strings(problemNames)

  group.Go(func() error {
    for _,name := range problemNames {
      select {
      case <-gCtx.Done(): return nil
      case problemsChan <- Problem{ps[name],name}:
      }
    }
    close(problemsChan)
    return nil
  })

  for i:=0; i<cores; i++ { group.Go(func() error {
    return worker(gCtx,timeout,problemsChan,resultsChan)
  })}

  group.Go(func() error {
    okCount := map[string]int{}
    for i:=0; i<len(ps); i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        scores := []string{}
        total := []string{}
        for _,prover := range provers {
          res := r.proverResults[prover.name]
          if res.Output.Solved {
            okCount[prover.name]++
            duration,err := ptypes.Duration(res.Duration)
            if err!=nil { return fmt.Errorf("ptypes.Duration(): %v",err) }
            scores = append(scores,fmt.Sprintf("%04.2fs",duration.Seconds()))
          } else {
            scores = append(scores,"-----")
          }
          total = append(total,fmt.Sprintf("%4d",okCount[prover.name]))
        }
        log.Printf("done %4d/%4d | %v | %v | %q",i+1,len(ps),strings.Join(total,"\t"),strings.Join(scores,"\t"),r.name)
      }
    }
    return nil
  })

  if err := group.Wait(); err!=nil {
    return fmt.Errorf("group.Wait(): %v",err)
  }

  return nil
}

func main() {
  flag.Parse()
  if err := run(context.Background(),4*time.Second,4); err!=nil {
    log.Fatalf("%v",err)
  }
}
