package main

import (
  "context"
  "fmt"
  "flag"
  "log"

  "google.golang.org/grpc"
  "google.golang.org/grpc/credentials"
  "google.golang.org/grpc/credentials/oauth"
  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
  pb "github.com/pompon0/tptp_benchmark_go/cloud/worker/worker_go_proto"
)

// run "gcloud auth application-default login" before executing this binary
var workerAddr = flag.String("worker_addr","worker-su5lpnpdhq-uc.a.run.app:443","worker service address")

func run(ctx context.Context) error {
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
  group,gCtx := errgroup.WithContext(ctx)
  for k,v := range problems.SampleProblems {
    k,v := k,v
    group.Go(func() error {
      log.Printf("problem %q",k)
      _,err := c.Prove(gCtx,&pb.Req{TptpProblem:v})
      if err!=nil {
        return fmt.Errorf("c.Prove(): %v",err)
      }
      log.Printf("OK")
      //log.Printf("resp = %v",resp)
      return nil
    })
  }
  if err := group.Wait(); err!=nil {
    return fmt.Errorf("group.Wait(): %v",err)
  }
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
