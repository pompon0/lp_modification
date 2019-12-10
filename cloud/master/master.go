package main

import (
  "context"
  "fmt"
  "flag"
  "log"

  "google.golang.org/grpc"
  "google.golang.org/grpc/credentials"
  "google.golang.org/grpc/credentials/oauth"
  pb "github.com/pompon0/tptp_benchmark_go/worker/worker_go_proto"
)

// run "gcloud auth application-default login" before executing this binary
var workerAddr = flag.String("worker_addr","worker-su5lpnpdhq-uc.a.run.app:443","worker service address")

func run(ctx context.Context) error {
  //err := credentials
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
  resp,err := c.Prove(ctx,&pb.Req{})
  if err!=nil {
    return fmt.Errorf("c.Prove(): %v",err)
  }
  log.Printf("resp = %v",resp)
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
