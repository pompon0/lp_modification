package main

import (
  "fmt"
  "context"
  "log"
  "net"
  "os"

  "google.golang.org/grpc"
  pb "github.com/pompon0/tptp_benchmark_go/worker/worker_go_proto"
)

type server struct {
}

func (*server) Prove(ctx context.Context, req *pb.Req) (*pb.Resp,error) {
  return nil,fmt.Errorf("unimplementeddd")
}

func run(ctx context.Context) error {
  port := os.Getenv("PORT")
  lis, err := net.Listen("tcp",":"+port)
  if err!=nil { return fmt.Errorf("net.Listen(): %v",err) }

  s := grpc.NewServer()
  pb.RegisterWorkerServer(s,&server{})
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
