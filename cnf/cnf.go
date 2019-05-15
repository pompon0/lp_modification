package main

import (
  "bytes"
  "fmt"
  "log"
  "context"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/problems"
)

const eprover_bin_path = "external/eprover/prover_bin"
const output_path = "/tmp/cnf_problem_set.tgz"

func run(ctx context.Context) error {
  before,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("GetProblems(): %v",err) }

  after := map[string][]byte{}

  for k,v := range before {
    var inBuf,outBuf,errBuf bytes.Buffer
    if _,err := inBuf.Write(v); err!=nil {
      return fmt.Errorf("inBuf(): %v",err)
    }
    cmd := exec.CommandContext(ctx,eprover_bin_path,"--cnf","-s")
    cmd.Stdin = &inBuf
    cmd.Stdout = &outBuf
    cmd.Stderr = &errBuf
    err := cmd.Run();
    if err!=nil {
      log.Printf("out = %q",outBuf.String())
      log.Printf("err = %q",errBuf.String())
      return fmt.Errorf("cmd.Run(): %v",err)
    }
    var buf bytes.Buffer
    for _,l := range strings.Split(outBuf.String(),"\n") {
      if l!="" && l[0]!='#' {
        buf.WriteString(l)
        buf.WriteByte('\n')
      }
    }
    after[k] = buf.Bytes()
  }
  if err := problems.WriteProblemSet(after,output_path); err!=nil {
    return fmt.Errorf("problems.WriteProblemSet(): %v",err)
  }
  return nil
}

func main() {
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
