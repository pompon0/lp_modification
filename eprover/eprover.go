package eprover

import (
  "bytes"
  "fmt"
  //"log"
  "context"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
)

const eproverBinPath = "eprover/prover_bin"
const resultOk = "# SZS status Theorem"

func Prove(ctx context.Context, tptpFOFProblem []byte) error {
  var inBuf,outBuf,errBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOFProblem); err!=nil {
    return fmt.Errorf("inBuf.Write(): %v",err)
  }

  cmd := exec.CommandContext(ctx,utils.Runfile(eproverBinPath),"-s")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err := cmd.Run(); err!=nil {
    //log.Printf("out = %q",outBuf.String())
    //log.Printf("err = %q",errBuf.String())
    return fmt.Errorf("cmd.Run(): %v",err)
  }
  lines := strings.Split(strings.TrimSpace(outBuf.String()),"\n")
  last := lines[len(lines)-1]
  if last!=resultOk { return fmt.Errorf("%s",last) }
  return nil
}

func FOFToCNF(ctx context.Context, tptpFOF []byte) ([]byte,error) {
  var inBuf,outBuf,errBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOF); err!=nil {
    return nil,fmt.Errorf("inBuf(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(eproverBinPath),"--cnf","-s")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err := cmd.Run(); err!=nil {
    //log.Printf("out = %q",outBuf.String())
    //log.Printf("err = %q",errBuf.String())
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }
  var buf bytes.Buffer
  for _,l := range strings.Split(outBuf.String(),"\n") {
    if l!="" && l[0]!='#' {
      buf.WriteString(l)
      buf.WriteByte('\n')
    }
  }
  return buf.Bytes(),nil
}

/*func run(ctx context.Context) error {
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
}*/
