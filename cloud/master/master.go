package main

import (
  "context"
  "fmt"
  "flag"
  "log"
  "time"
  "os"
  "sort"
  "strings"

  "google.golang.org/grpc"
  "google.golang.org/grpc/credentials"
  "google.golang.org/grpc/credentials/oauth"
  "google.golang.org/grpc/status"
  "google.golang.org/grpc/codes"
  "golang.org/x/sync/semaphore"
  "golang.org/x/sync/errgroup"
  "github.com/golang/protobuf/ptypes"

  "github.com/pompon0/tptp_benchmark_go/problems"
  pb "github.com/pompon0/tptp_benchmark_go/cloud/worker/worker_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

////////////////////////////////////////////

// run "gcloud auth application-default login" before executing this binary
var workerAddr = flag.String("worker_addr","worker-su5lpnpdhq-uc.a.run.app:443","worker service address")

//var proofDir = flag.String("proof_dir","/tmp/benchmark_proofs","output directory to write proofs to")

var reportDir = flag.String("report_dir","","")

var maxInFlight = flag.Int("max_in_flight",10,"")
var problemLimit = flag.Int("problem_limit",-1,"number of problems to solve (-1 for all problems)")
//var unsolvedOnly = flag.Bool("unsolved_only",false,"process only unsolved problems")
var timeout = flag.Duration("timeout",16*time.Second,"timeout per problem")


type EnumFlag struct {
  fromString map[string]int32
  Value int32
}

func NewEnumFlag(fromString map[string]int32) *EnumFlag {
  return &EnumFlag{fromString: fromString}
}

func (f *EnumFlag) Desc() string {
  var names []string
  for k,_ := range f.fromString { names = append(names,k) }
  sort.Slice(names,func(i,j int) bool { return f.fromString[names[i]] < f.fromString[names[j]] })
  return strings.Join(names,"|")
}

func (f *EnumFlag) Set(name string) error {
  v,ok := f.fromString[name]
  if !ok { return fmt.Errorf("unknown %q",name) }
  f.Value = v
  return nil
}

func (f *EnumFlag) String() string {
  for k,v := range f.fromString { if v==f.Value { return k } }
  // For some reason we need to support execution on nil
  // (https://golang.org/pkg/flag/#Value)
  return ""
}

const (
  problemSetMizar = "mizar"
  problemSetTptp = "tptp"
)
var problemSets = []string{problemSetMizar,problemSetTptp}

var problemSet = flag.String("problem_set",problemSetMizar,strings.Join(problemSets,"|"))
var prover = NewEnumFlag(pb.Prover_value)

func run(ctx context.Context) error {
  // connect to worker pool
  creds,err := oauth.NewApplicationDefault(ctx)
  if err!=nil {
    return fmt.Errorf("oauth.NewApplicationDefault(): %v",err)
  }
  tc := credentials.NewTLS(nil)
  conn, err := grpc.Dial(*workerAddr,
    grpc.WithPerRPCCredentials(creds),
    grpc.WithTransportCredentials(tc))
  if err != nil {
    return fmt.Errorf("grpc.Dial(): %v", err)
  }
  defer conn.Close()
  c := pb.NewWorkerClient(conn)


  // start a report
  if _,err := os.Stat(*reportDir); os.IsNotExist(err) {
    return fmt.Errorf("report_dir = %q doesn't exist",*reportDir)
  }
  /*if _,err := os.Stat(*proofDir); os.IsNotExist(err) {
    return fmt.Errorf("proof_dir = %q doesn't exist",*proofDir)
  }*/
  date,err := ptypes.TimestampProto(time.Now())
  if err!=nil {
    return fmt.Errorf("ptypes.TimestampProto(): %v",err)
  }
  report := &spb.Report {
    Date: date,
    Commit: "(worker)",
  }

  var prob map[string]*problems.Problem
  switch *problemSet {
  case problemSetMizar:
    x,cancel,err := problems.MizarProblems()
    if err!=nil { return fmt.Errorf("problems.MizarProblems(): %v",err) }
    defer cancel()
    prob = x
  case problemSetTptp:
    x,cancel,err := problems.TptpProblems()
    if err!=nil { return fmt.Errorf("problems.TptpProblems(): %v",err) }
    defer cancel()
    prob = x
  default:
    return fmt.Errorf("unknown problem set %q",*problemSet)
  }

  resultsChan := make(chan *spb.Case,16)
  group,gCtx := errgroup.WithContext(ctx)

  var probNames []string
  for name,_ := range prob { probNames = append(probNames,name) }
  sort.Strings(probNames)
  if *problemLimit>=0 { probNames = probNames[:*problemLimit] }

  timeoutProto := ptypes.DurationProto(*timeout)
  inFlightSem := semaphore.NewWeighted(int64(*maxInFlight))
  for _,name := range probNames {
    name := name
    group.Go(func() error {
      tptp,err := prob[name].Get()
      if err!=nil { return fmt.Errorf("prob[%q].Get(): %v",err) }

      // throttle in-flight operations
      if err:=inFlightSem.Acquire(gCtx,1); err!=nil {
        return fmt.Errorf("inFlightSem.Acquire(): %v",err)
      }
      defer inFlightSem.Release(1)

      var resp *pb.Resp
      for {
        var err error
        resp,err = c.Prove(gCtx,&pb.Req{
          Prover: pb.Prover(prover.Value),
          TptpProblem: tptp,
          Timeout: timeoutProto,
        })
        if err==nil { break }
        st := status.Convert(err)
        log.Printf("%+v\n",st.Proto())
        for _,d := range st.Details() {
          log.Printf("detail: %+v\n",d)
        }
        switch st.Code() {
        case codes.Unavailable:
        case codes.Internal:
        default: return fmt.Errorf("c.Prove(%q): %v",name,err)
        }
        time.Sleep(2)
      }
      resp.Case.Name = name
      if resp.Case.Output==nil {
        return fmt.Errorf("prob[%q]: missing output",name)
      }
      select {
      case <-gCtx.Done(): return nil
      case resultsChan <- resp.Case: return nil
      }
    })
  }

  group.Go(func() error {
    okCount := 0
    failCount := 0
    for i:=0; i<len(probNames); i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        report.Cases = append(report.Cases, r)
        if !r.Output.Solved {
          failCount++
        } else {
          //legacyProof := &spb.CNF{Problem:r.CnfProblem, Proof:r.Output.Proof}
          /*isNew,err := problems.WriteProof(*proofDir,r.Name,legacyProof)
          if err!=nil {
            return fmt.Errorf("writeProof(): %v",err)
          }
          if isNew { newCount++ }*/
          okCount++
        }
        log.Printf("done %v/%v fail=%v ok=%v",i+1,len(probNames),failCount,okCount)
      }
    }
    return nil
  })

  if err := group.Wait(); err!=nil {
    return fmt.Errorf("group.Wait(): %v",err)
  }

  log.Printf("writing report...")
  if err := problems.WriteReport(*reportDir,report); err!=nil { return fmt.Errorf("WriteReport(): %v",err) }

  return nil
}

func main() {
  flag.Var(prover,"prover",prover.Desc())
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
