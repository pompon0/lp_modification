package eprover

import (
  "bytes"
  "fmt"
  "log"
  "context"
  "os"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const eproverBinPath = "eprover/prover_bin"
const statusPrefix = "# SZS status "
// proved
const statusTheorem = "Theorem"
const statusUnsatisfiable = "Unsatisfiable"
const statusContradictoryAxioms = "ContradictoryAxioms"
// refuted
const statusCounterSatisfiable = "CounterSatisfiable"

const resultOk = "# SZS status Theorem"

func Prove(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  const memLimitBytes = 2000000000 // 2GB
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOFProblem); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }

  cmd := exec.CommandContext(ctx,utils.Runfile(eproverBinPath),"-s","--auto-schedule")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err := utils.RunWithMemLimit(cmd,memLimitBytes); err!=nil {
    if ctx.Err()==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    if err.(*exec.ExitError).ExitCode()==9 {
      return &spb.ProverOutput{Solved:false},nil
    }
    //log.Printf("out = %q",outBuf.String())
    //log.Printf("err = %q",errBuf.String())
    return nil,fmt.Errorf("cmd.Run(): %q %v",outBuf.String(),err)
  }
  for _,l := range strings.Split(strings.TrimSpace(outBuf.String()),"\n") {
    if strings.HasPrefix(l,statusPrefix) {
      switch status := strings.Split(strings.TrimPrefix(l,statusPrefix)," ")[0]; status {
      case statusTheorem:
      case statusUnsatisfiable:
      case statusContradictoryAxioms:
      default: return nil,fmt.Errorf("unknown status %q",status)
      }
      return &spb.ProverOutput{Solved:true},nil
    }
  }
  return nil,fmt.Errorf("status line not found")
}

func FOFToCNF(ctx context.Context, tptpFOF []byte) ([]byte,error) {
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOF); err!=nil {
    return nil,fmt.Errorf("inBuf(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(eproverBinPath),"--cnf","-s")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err := cmd.Run(); err!=nil {
    if ctx.Err()!=nil { return nil,ctx.Err() }
    log.Printf("in = %q",tptpFOF)
    log.Printf("out = %q",outBuf.String())
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
