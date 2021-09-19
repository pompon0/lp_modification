package leancop

import (
  "bytes"
  "fmt"
  "context"
  "log"
  "os/exec"
  "strings"
  "syscall"

  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const leancopBinPath = "leancop/bmtp"
const resultOk = "% SZS status Theorem"
const resultUnknown = "%C SZS status Unknown"
const resultNoConjecture = "%No conjecture or negated conjecture"

const oom = "Fatal error: out of memory."

func Prove(ctx context.Context, fofProblem *tool.TPTP) (*spb.ProverOutput,error) {
  tmp,cleanup,err := tool.WriteTmp(fofProblem.Raw)
  if err!=nil { return nil,fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()

  const memLimitBytes = 2000000000 // 2GB
  var inBuf,outBuf,errBuf bytes.Buffer
  cmd := exec.Command(utils.Runfile(leancopBinPath),tmp)
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err := utils.RunWithMemLimit(ctx,cmd,memLimitBytes); err!=nil {
    status := err.(*exec.ExitError).Sys().(syscall.WaitStatus)
    log.Printf("status = %v",status)
    if status.Signaled() && status.Signal()==syscall.SIGSEGV {
      return &spb.ProverOutput{Solved:false},nil
    }
    if strings.TrimSpace(errBuf.String())==oom {
      return &spb.ProverOutput{Solved:false},nil
    }
    if ctx.Err()==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    return nil,fmt.Errorf("cmd.Run(): %q %v",errBuf.String(),err)
  }
  lines := strings.Split(strings.TrimSpace(outBuf.String()),"\n")
  last := lines[len(lines)-1]
  switch last {
  case resultOk:
    return &spb.ProverOutput{Solved:true},nil
  case resultUnknown:
    return &spb.ProverOutput{Solved:false},nil
  case resultNoConjecture:
    return &spb.ProverOutput{Solved:false},nil
  default:
    return nil,fmt.Errorf("%s",last)
  }
}

