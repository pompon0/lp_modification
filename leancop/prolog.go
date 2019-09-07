package leancop

import (
  "fmt"
  "bytes"
  "context"
  "os/exec"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

const swiplBinPath = "/usr/bin/swipl"
const leancopMainPath = "leancop_prolog/leancop21/leancop_main.pl"

func PrologProve(ctx context.Context, tptpFOFProblem []byte) error {
  tmp,cleanup,err := tool.WriteTmp(tptpFOFProblem)
  if err!=nil { return fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()
  var inBuf,outBuf,errBuf bytes.Buffer
  program := `
    assert((print(A):-write(A))),
    assert(prolog(swi)),
    assert(proof(compact)),
    ['%s'],
    leancop_main('%s',[def],_).`
  swiplArgs := []string {
    "-t",
    "halt",
    "-nodebug",
    "-L120M",
    "-G120M",
    "-T100M",
    "-q",
    "-g",
    fmt.Sprintf(program,utils.Runfile(leancopMainPath),tmp),
  }
  cmd := exec.CommandContext(ctx,swiplBinPath,swiplArgs...)
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err := cmd.Run(); err!=nil {
    return fmt.Errorf("cmd.Run(): %v",err)
  }
  lines := strings.Split(strings.TrimSpace(outBuf.String()),"\n")
  want := fmt.Sprintf("%s is a Theorem",tmp)
  if len(lines)<1 || lines[0]!=want {
    return fmt.Errorf("%s",lines[0])
  }
  return nil
}
