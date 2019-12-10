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
  "github.com/golang/protobuf/ptypes"

  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/tableau"
  "github.com/pompon0/tptp_benchmark_go/tool"
  pb "github.com/pompon0/tptp_benchmark_go/worker/worker_go_proto"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

type server struct {
  sem *semaphore.Weighted
}

func NewServer() *server {
  return &server{
    sem: semaphore.NewWeighted(1),
  }
}

const timeout = 30*time.Second

func (s *server) Prove(ctx context.Context, req *pb.Req) (*pb.Resp,error) {
  ok := s.sem.TryAcquire(1)
  if !ok { return nil,status.New(codes.ResourceExhausted,"too many requests").Err() }
  defer s.sem.Release(1)

  prover := tableau.Tableau
  fofProblem,cnfProblem,err := convProblem(ctx,req.TptpProblem)
  if err!=nil { return nil,status.Newf(codes.InvalidArgument,"convProblem(): %v",err).Err() }
  proverCtx,cancel := context.WithTimeout(ctx,timeout)
  defer cancel()

  c := &spb.Case{
    FofProblem: fofProblem,
    CnfProblem: cnfProblem,
  }
  t0 := time.Now()
  out,err := prover(proverCtx,cnfProblem,false,true)
  c.Duration = ptypes.DurationProto(time.Since(t0))
  if err==nil {
    c.Output = out
    if out.Proof!=nil {
      if _,err := tool.ValidateProof(ctx,&spb.CNF{Problem:c.CnfProblem,Proof:out.Proof}); err!=nil {
        return nil,status.Newf(codes.Internal,"tool.ValidateProof(): %v",err).Err()
      }
    }
  }
  return &pb.Resp{Case:c},nil
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

func run(ctx context.Context) error {
  port := os.Getenv("PORT")
  lis, err := net.Listen("tcp","0.0.0.0:"+port)
  if err!=nil { return fmt.Errorf("net.Listen(): %v",err) }

  s := grpc.NewServer()
  pb.RegisterWorkerServer(s,NewServer())
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
