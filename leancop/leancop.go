package leancop

import (
  "bytes"
  "fmt"
  //"log"
  "context"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const leancopBinPath = "leancop/bmtp"
const resultOk = "% SZS status Theorem"
const resultUnknown = "%C SZS status Unknown"
func Prove(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  tmp,cleanup,err := tool.WriteTmp(tptpFOFProblem)
  if err!=nil { return nil,fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()

  var inBuf,outBuf,errBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(leancopBinPath),tmp)
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err := cmd.Run(); err!=nil {
    if ctx.Err()==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }
  lines := strings.Split(strings.TrimSpace(outBuf.String()),"\n")
  last := lines[len(lines)-1]
  switch last {
  case resultOk:
    return &spb.ProverOutput{Solved:true},nil
  case resultUnknown:
    return &spb.ProverOutput{Solved:false},nil
  default:
    return nil,fmt.Errorf("%s",last)
  }
}

