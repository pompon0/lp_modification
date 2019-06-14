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
const sample_output_path = "/tmp/cnf_sample.tgz"


func toCNF(ctx context.Context, before map[string][]byte) (map[string][]byte,error) {
  after := map[string][]byte{}

  for k,v := range before {
    var inBuf,outBuf,errBuf bytes.Buffer
    if _,err := inBuf.Write(v); err!=nil {
      return nil,fmt.Errorf("inBuf(): %v",err)
    }
    cmd := exec.CommandContext(ctx,eprover_bin_path,"--cnf","-s")
    cmd.Stdin = &inBuf
    cmd.Stdout = &outBuf
    cmd.Stderr = &errBuf
    err := cmd.Run();
    if err!=nil {
      log.Printf("out = %q",outBuf.String())
      log.Printf("err = %q",errBuf.String())
      return nil,fmt.Errorf("cmd.Run(): %v",err)
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
  return after,nil
}

func run(ctx context.Context) error {
  // benchmark problem set
  before,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("GetProblems(): %v",err) }
  after,err := toCNF(ctx,before)
  if err!=nil { return fmt.Errorf("toCNF(): %v",err) }
  if err := problems.WriteProblemSet(after,output_path); err!=nil {
    return fmt.Errorf("problems.WriteProblemSet(): %v",err)
  }

  // sample problem set
  before = problems.SampleProblems
  after,err = toCNF(ctx,before)
  if err!=nil { return fmt.Errorf("toCNF(): %v",err) }
  if err := problems.WriteProblemSet(after,sample_output_path); err!=nil {
    return fmt.Errorf("problems.WriteProblemSet(): %v",err)
  }

  return nil
}

func main() {
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
