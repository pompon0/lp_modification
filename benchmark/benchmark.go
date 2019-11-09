package main

import (
  "fmt"
  "log"
  "time"
  "sort"
  "strings"
  "flag"
  "context"
  "os"

  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/golang/protobuf/ptypes"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

type Prover func(ctx context.Context, cnfProblem *tpb.File, streamStdErr bool) (*spb.ProverOutput, error)

type Problem struct {
  name string
  tptpFOFProblem []byte
}

type Result struct {
  case_ *spb.Case
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
      fofProblem,cnfProblem,err := convProblem(ctx,p.tptpFOFProblem)
      if err!=nil { return fmt.Errorf("convProblem(%q): %v",p.name,err) }
      if err := func() error {
        c := &spb.Case{
          Name: p.name,
          FofProblem: fofProblem,
          CnfProblem: cnfProblem,
        }
        proverCtx,cancel := context.WithTimeout(ctx,timeout)
        defer cancel()
        t0 := time.Now()
        out,err := prover(proverCtx,c.CnfProblem,false)
        c.Duration = ptypes.DurationProto(time.Since(t0))
        if err==nil {
          c.Output = out
          if _,err := tool.ValidateProof(ctx,&spb.CNF{Problem:c.CnfProblem,Proof:out.Proof}); err!=nil { return err }
        }
        results <- Result{c,err}
        return nil
      }(); err!=nil { return err }
    }
  }
}

func convProblem(ctx context.Context, tptp []byte) (/*fof*/ *tpb.File, /*cnf*/ *tpb.File, error) {
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
  if err!=nil { return nil,nil,fmt.Errorf("tool.TptpToProto(): %v",err) }
  cnf,err := tool.FOFToCNF(ctx,fof)
  if err!=nil { return nil,nil,fmt.Errorf("tool.FOFTOCNF(): %v",err) }
  return fof,cnf,nil
}

func convProblemEprover(ctx context.Context, tptp []byte) (/*fof*/ *tpb.File, /*cnf*/ *tpb.File, error) {
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptp)
  if err!=nil { return nil,nil,fmt.Errorf("tool.TptpToProto(FOF): %v",err) }
  tptpCNF,err := eprover.FOFToCNF(ctx,tptp)
  if err!=nil { return nil,nil,fmt.Errorf("eprover.FOFToCNF(): %v",err) }
  cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
  if err!=nil { return nil,nil,fmt.Errorf("tool.TptpToProto(CNF): %v",err) }
  return fof,cnf,nil
}

////////////////////////////////////////////

var proofDir = flag.String("proof_dir","/tmp/benchmark_proofs","output directory to write proofs to")

var reportDir = flag.String("report_dir","","")
var commit = flag.String("commit","","sha256 of a current commit")
var labels = flag.String("labels","","comma separated list of labels")

var unsolvedOnly = flag.Bool("unsolved_only",false,"process only unsolved problems")
var timeout = flag.Duration("timeout",16*time.Second,"timeout per problem")

const cores = 4
const mod = 1

func run(ctx context.Context) error {
  if *commit=="" { return fmt.Errorf("commit has to be nonempty") }
  if _,err := os.Stat(*reportDir); os.IsNotExist(err) {
    return fmt.Errorf("report_dir = %q doesn't exist",*reportDir)
  }
  if _,err := os.Stat(*proofDir); os.IsNotExist(err) {
    return fmt.Errorf("proof_dir = %q doesn't exist",*proofDir)
  }
  date,err := ptypes.TimestampProto(time.Now())
  if err!=nil {
    return fmt.Errorf("ptypes.TimestampProto(): %v",err)
  }
  report := &spb.Report {
    Date: date,
    Commit: *commit,
    Labels: strings.Split(*labels,","),
  }

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
    return worker(gCtx,*timeout,tableau.Tableau,probChan,resultsChan)
  })}

  group.Go(func() error {
    okCount := 0
    newCount := 0
    errCount := 0
    for i:=0; i<probCount; i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        report.Cases = append(report.Cases, r.case_)
        if r.err!=nil {
          log.Printf("cnfProblem[%q]: %v",r.case_.Name,r.err)
          errCount++
        } else {
          legacyProof := &spb.CNF{Problem:r.case_.CnfProblem, Proof:r.case_.Output.Proof}
          isNew,err := problems.WriteProof(*proofDir,r.case_.Name,legacyProof)
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

  log.Printf("writing report...")
  if err := problems.WriteReport(*reportDir,report); err!=nil { return fmt.Errorf("WriteReport(): %v",err) }

  return nil
}

func main() {
  flag.Parse()
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
