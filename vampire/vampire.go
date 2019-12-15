package vampire

import (
  "bytes"
  "fmt"
  //"log"
  "context"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
)

const vampireBinPath = "__main__/vampire/vampire"
const resultOk = "% SZS status Theorem for"

func Prove(ctx context.Context, tptpFOFProblem []byte) error {
  var inBuf,outBuf,errBuf bytes.Buffer
  if _,err := inBuf.Write(tptpFOFProblem); err!=nil {
    return fmt.Errorf("inBuf.Write(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(vampireBinPath),
    "--statistics","none",
    "--proof","off")
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
  if !strings.HasPrefix(last,resultOk) { return fmt.Errorf("%s",last) }
  return nil
}

