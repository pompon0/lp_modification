package vampire

import (
  "bytes"
  "fmt"
  //"log"
  "os"
  "context"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const vampireBinPath = "vampire/vampire"
const statusPrefix = "% SZS status "
// proved
const statusTheorem = "Theorem"
const statusUnsatisfiable = "Unsatisfiable"
const statusContradictoryAxioms = "ContradictoryAxioms"
// refuted
const statusCounterSatisfiable = "CounterSatisfiable"
// failed
const refutationNotFound = "% Refutation not found"

func Prove(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  const memLimitBytes = 2000000000 // 2GB
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOFProblem); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }
  cmd := exec.Command(
    utils.Runfile(vampireBinPath),
    "--statistics","none",
    "--proof","off",
    "--mode","casc")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err := utils.RunWithMemLimit(ctx,cmd,memLimitBytes); err!=nil {
    if ctx.Err()==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    if strings.HasPrefix(outBuf.String(),refutationNotFound) {
      return &spb.ProverOutput{Solved:false},nil
    }
    if err.(*exec.ExitError).ExitCode() == 2 {
      return &spb.ProverOutput{Solved:false},nil
    }
    //log.Printf("out = %s",outBuf.String())
    //log.Printf("err = %s",errBuf.String())
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
  //log.Printf("out = %s",outBuf.String())
  return nil,fmt.Errorf("status line not found")
}

