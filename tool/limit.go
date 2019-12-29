package tool

import (
  "fmt"
  "errors"
  "os/exec"

  "github.com/pompon0/tptp_benchmark_go/utils"
)

const limitBinPath = "__main__/tool/limit"
const oom = "Fatal error: out of memory."

var ErrOutOfMemory = errors.New("Out of memory")

func RunWithMemLimit(cmd *exec.Cmd, limitBytes int) error {
  cmd2 := *cmd
  cmd2.Path = utils.Runfile(limitBinPath)
  cmd2.Args = append([]string{cmd2.Path,fmt.Sprintf("%d",limitBytes)},cmd.Args...)
  return (&cmd2).Run()
}
