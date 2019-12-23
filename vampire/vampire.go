package vampire

import (
  "bytes"
  "fmt"
  //"log"
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
  var inBuf,outBuf,errBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOFProblem); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(vampireBinPath),
    "--statistics","none",
    "--proof","off")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err := cmd.Run(); err!=nil {
    if ctx.Err()==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    if strings.HasPrefix(outBuf.String(),refutationNotFound) {
      return &spb.ProverOutput{Solved:false},nil
    }
    //log.Printf("out = %s",outBuf.String())
    //log.Printf("err = %s",errBuf.String())
    return nil,fmt.Errorf("cmd.Run(): %v",err)
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

