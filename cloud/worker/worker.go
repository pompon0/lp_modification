package main

import (
  "fmt"
  "context"
  "log"
  "net"
  "os"
  "time"

  "google.golang.org/grpc"
  "google.golang.org/grpc/status"
  "google.golang.org/grpc/codes"
  "golang.org/x/sync/semaphore"
  "google.golang.org/protobuf/types/known/durationpb"

  "github.com/pompon0/tptp_benchmark_go/vampire"
  "github.com/pompon0/tptp_benchmark_go/leancop"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/ffprover"
  pb "github.com/pompon0/tptp_benchmark_go/cloud/worker/worker_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

type server struct {
  sem *semaphore.Weighted
  commit string
}

func NewServer(commit string) *server {
  return &server{
    sem: semaphore.NewWeighted(1),
    commit: commit,
  }
}

const maxTimeout = 61*time.Second

func (s *server) Prove(ctx context.Context, req *pb.Req) (*pb.Resp,error) {
  ok := s.sem.TryAcquire(1)
  if !ok { return nil,status.New(codes.ResourceExhausted,"too many requests").Err() }
  defer s.sem.Release(1)

  if got,want := req.GetCommit(),s.commit; got!=want {
    return nil,status.Newf(codes.Unavailable,"commit got = %q, want %q",got,want).Err()
  }

  timeout := req.Timeout.AsDuration()
  proverCtx,cancel := context.WithTimeout(ctx,timeout)
  defer cancel()

  c := &spb.Case{}
  t0 := time.Now()
  var err error
  switch req.Prover {
    case pb.Prover_VAMPIRE:
      c.Output,err = vampire.Prove(proverCtx,req.TptpProblem)
    case pb.Prover_VAMPIRE_NO_EQ:
      c.Output,err = vampire.ProveNoEq(proverCtx,req.TptpProblem)
    case pb.Prover_EPROVER:
      c.Output,err = eprover.Prove(proverCtx,req.TptpProblem)
    case pb.Prover_LEANCOP_BMTP:
      c.Output,err = leancop.Prove(proverCtx,req.TptpProblem)
    case pb.Prover_LEANCOP_PROLOG:
      c.Output,err = leancop.PrologProve(proverCtx,req.TptpProblem)
    case pb.Prover_LEANCOP_PROLOG_CUT_COMP_7:
      c.Output,err = leancop.PrologProveCutComp7(proverCtx,req.TptpProblem)
    case pb.Prover_GPRUSAK_AXIOMATIC_EQ:
      c.Output,err = tableau.ProveAxiomaticEq(proverCtx,req.TptpProblem)
    case pb.Prover_GPRUSAK_LP_MODIFICATION:
      c.Output,err = tableau.ProveLPModification(proverCtx,req.TptpProblem)
    case pb.Prover_GPRUSAK_LP_MODIFICATION_DEPTH:
      c.Output,err = tableau.ProveLPModificationDepth(proverCtx,req.TptpProblem)
    case pb.Prover_GPRUSAK_LAZY_PARAMODULATION:
      c.Output,err = tableau.ProveLazyParamodulation(proverCtx,req.TptpProblem)
    case pb.Prover_GPRUSAK_NO_EQ:
      c.Output,err = tableau.ProveNoEq(proverCtx,req.TptpProblem)
    case pb.Prover_GPRUSAK_MCTS_FULL_SEARCH:
      c.Output,err = mcts.Prove(proverCtx,req.TptpProblem)
    default:
      return nil,status.New(codes.InvalidArgument,"unknown prover").Err()
  }
  if err!=nil { return nil,status.Newf(codes.Internal,"prove(): %v",err).Err() }
  c.Duration = durationpb.New(time.Since(t0))
  if req.ValidateProof && c.Output.Proof!=nil {
    if _,err := tool.ValidateProof(ctx,&spb.CNF{Problem:c.Output.CnfProblem,Proof:c.Output.Proof}); err!=nil {
      return nil,status.Newf(codes.Internal,"tool.ValidateProof(): %v",err).Err()
    }
  }
  // clear the heavy fields, because response size limit is 32MB
  // You can store heavy response in GCS if needed.
  c.Output.CnfProblem = nil
  c.Output.Proof = nil
  return &pb.Resp{Case:c},nil
}

func run(ctx context.Context) error {
  commit := os.Getenv("COMMIT")
  if commit=="" { return fmt.Errorf("COMMIT cannot be empty") }

  port := os.Getenv("PORT")
  lis, err := net.Listen("tcp","0.0.0.0:"+port)
  if err!=nil { return fmt.Errorf("net.Listen(): %v",err) }

  s := grpc.NewServer()
  pb.RegisterWorkerServer(s,NewServer(commit))
  if err := s.Serve(lis); err!=nil {
    return fmt.Errorf("s.Serve(): %v",err)
  }
  return nil
}

func main() {
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
