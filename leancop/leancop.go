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
)

const leancopBinPath = "leancop/bmtp"
const resultOk = "% SZS status Theorem"

func Prove(ctx context.Context, tptpFOFProblem []byte) error {
  tmp,cleanup,err := tool.WriteTmp(tptpFOFProblem)
  if err!=nil { return fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()

  var inBuf,outBuf,errBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(leancopBinPath),tmp)
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

