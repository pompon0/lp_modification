package main

import(
  "fmt"
  "log"
  "flag"
  "context"

  "github.com/pompon0/tptp_benchmark_go/problems"
)

var proofDir = flag.String("proof_dir","","")

func run(ctx context.Context) error {
  ctx := context.Background()
  prob,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("problems.GetProblems(): %v",err) }
  for name,_ := range prob {
    proofs,err := problems.ReadProofs(*proofDir,name)
    if err!=nil {
      return fmt.Errorf("problems.ReadProofs(%q): %v",name,err) }
    }
  }
  return nil
}

func main() {
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
