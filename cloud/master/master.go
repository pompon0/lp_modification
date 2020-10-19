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
  "sync/atomic"

  "google.golang.org/grpc"
  "google.golang.org/grpc/credentials"
  "google.golang.org/grpc/credentials/oauth"
  "google.golang.org/grpc/status"
  "google.golang.org/grpc/codes"
  "golang.org/x/sync/semaphore"
  "golang.org/x/sync/errgroup"
  "github.com/golang/protobuf/ptypes"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/cloud/worker/push"
  pb "github.com/pompon0/tptp_benchmark_go/cloud/worker/worker_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

////////////////////////////////////////////

// run "gcloud auth configure-docker", so that binary is able to push to container registry
// run "gcloud auth application-default login" so that binary has access to GCP.
var workerAddr = flag.String("worker_addr","worker-su5lpnpdhq-uc.a.run.app:443","worker service address")
// add a large subordinate uid & gid range to the user
// podman --cgroup=cgroupfs --log-level=debug --runtime=crun run -e COMMIT=xoxo -e PORT=6455 -p 5757:6455
// worker_addr=localhost:5757
var useTLS = flag.Bool("use_tls",true,"")
var pushImage = flag.Bool("push_image",true,"")

var reportDir = flag.String("report_dir","","")
var verboseLogPath = flag.String("verbose_log_path","/tmp/master.log","")

var maxInFlight = flag.Int("max_in_flight",10,"")
var problemLimit = flag.Int("problem_limit",-1,"number of problems to solve (-1 for all problems)")
var timeout = flag.Duration("timeout",16*time.Second,"timeout per problem")

var validateProof = flag.Bool("validate_proof",true,"")
var equalityOnly = flag.Bool("equality_only",false,"")

const (
  problemSetMizar = "mizar"
  problemSetTptp = "tptp"
)

var inFlight int64
var problemSets = []string{problemSetMizar,problemSetTptp}

var commit = flag.String("commit","","current version")
var problemSet = flag.String("problem_set",problemSetMizar,strings.Join(problemSets,"|"))
var prover = (*pb.Prover)(utils.NewEnumFlag("prover",pb.Prover_UNKNOWN))

type ConnPool struct {
  conn []*grpc.ClientConn
  next int64
}

func (p *ConnPool) Get() *grpc.ClientConn {
  return p.conn[atomic.AddInt64(&p.next,1)%int64(len(p.conn))]
}

func NewConnPool(ctx context.Context, addr string, n int) (*ConnPool,func(),error) {
  if n<1 { return nil,nil,fmt.Errorf("n = %d",n) }
  p := &ConnPool{}
  cancel := func() { for _,c := range p.conn { c.Close() } }
  // connect to worker pool
  opts := []grpc.DialOption{grpc.WithInsecure()}
  if *useTLS {
    creds,err := oauth.NewApplicationDefault(ctx)
    if err!=nil {
      cancel()
      return nil,nil,fmt.Errorf("oauth.NewApplicationDefault(): %v",err)
    }
    tc := credentials.NewTLS(nil)
    opts = []grpc.DialOption{
      grpc.WithPerRPCCredentials(creds),
      grpc.WithTransportCredentials(tc),
    }
  }
  for i:=0; i<n; i++ {
    conn, err := grpc.Dial(addr,opts...)
    if err != nil {
      cancel()
      return nil,nil,fmt.Errorf("grpc.Dial(): %v", err)
    }
    p.conn = append(p.conn,conn)
  }
  return p,cancel,nil
}

func run(ctx context.Context) error {
  if *commit=="" {
    return fmt.Errorf("commit cannot be empty")
  }
  // deploy current revision
  if *pushImage {
    if err:=push.Push(ctx,*commit); err!=nil {
      return fmt.Errorf("push.Push(): %v",err)
    }
  }

  pool,cancel,err := NewConnPool(ctx,*workerAddr,15)
  if err!=nil { return fmt.Errorf("NewConnPool(%q): %v",*workerAddr,err) }
  defer cancel()

  // start a report
  if _,err := os.Stat(*reportDir); os.IsNotExist(err) {
    return fmt.Errorf("report_dir = %q doesn't exist",*reportDir)
  }
  date,err := ptypes.TimestampProto(time.Now())
  if err!=nil {
    return fmt.Errorf("ptypes.TimestampProto(): %v",err)
  }
  report := &spb.Report {
    Date: date,
    Commit: *commit,
    Labels: []string {
      fmt.Sprintf("--problem_set=%s",*problemSet),
      fmt.Sprintf("--prover=%s",prover),
    },
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
    if err!=nil { return fmt.Errorf("problems.TptpProblems(): %w",err) }
    defer cancel()
    prob = x
  default:
    return fmt.Errorf("unknown problem set %q",*problemSet)
  }

  vlog,err := os.Create(*verboseLogPath)
  if err!=nil { return fmt.Errorf("os.Create(): %w",err) }

  resultsChan := make(chan *spb.Case,1000)
  group,gCtx := errgroup.WithContext(ctx)

  var probNames []string
  for name,_ := range prob { probNames = append(probNames,name) }
  sort.Strings(probNames)
  if *problemLimit>=0 { probNames = probNames[:*problemLimit] }

  timeoutProto := ptypes.DurationProto(*timeout)
  inFlightSem := semaphore.NewWeighted(int64(*maxInFlight))
  processSem := semaphore.NewWeighted(int64(100))
  for _,name := range probNames {
    name := name
    group.Go(func() error {
      tptp,err := prob[name].Get()
      if err!=nil { return fmt.Errorf("prob[%q].Get(): %v",err) }

      if *equalityOnly {
        if err:=processSem.Acquire(gCtx,1); err!=nil {
          return fmt.Errorf("processSem.Acquire(): %v",err)
        }
        has := false
        for {
          var err error
          has,err = tool.TptpHasEquality(gCtx,tptp)
          if err==nil { break }
        }
        processSem.Release(1)
        if err!=nil {
          return fmt.Errorf("tool.HasEquality(%q): %v",name,err)
        }
        if !has {
          // send to signal that case has been filtered out.
          select {
          case <-gCtx.Done(): return nil
          case resultsChan <- nil: return nil
          }
          return nil
        }
      }

      // throttle in-flight operations
      if err:=inFlightSem.Acquire(gCtx,1); err!=nil {
        return fmt.Errorf("inFlightSem.Acquire(): %v",err)
      }
      defer inFlightSem.Release(1)

      var resp *pb.Resp
      for {
        var err error
        atomic.AddInt64(&inFlight,1)
        c := pb.NewWorkerClient(pool.Get())
        //t := time.Now()
        resp,err = c.Prove(gCtx,&pb.Req{
          Commit: *commit,
          Prover: *prover,
          TptpProblem: tptp,
          Timeout: timeoutProto,
          ValidateProof: *validateProof,
        })
        // log.Printf("latency = %v",time.Now().Sub(t))
        atomic.AddInt64(&inFlight,-1)
        if err==nil { break }
        st := status.Convert(err)
        log.Printf("c.Prove(%q): %+v\n",name,st.Proto())
        for _,d := range st.Details() {
          log.Printf("detail: %+v\n",d)
        }
        switch st.Code() {
        case codes.Unknown:
        case codes.Unavailable:
        case codes.Internal: // probably bug
        default: return fmt.Errorf("c.Prove(%q): %v",name,err)
        }
        time.Sleep(4)
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
    filteredCount := 0
    for i:=0; i<len(probNames); i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r==nil {
          filteredCount++
          continue
        }
        if _,err:=vlog.WriteString(fmt.Sprintf("%+v\n",r)); err!=nil {
          return fmt.Errorf("vlog.WriteString(): %w",err)
        }
        report.Cases = append(report.Cases, r)
        if !r.Output.Solved {
          failCount++
        } else {
          okCount++
        }
        log.Printf("done %v/%v fail=%v ok=%v filtered=%v",i+1,len(probNames),failCount,okCount,filteredCount)
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
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
