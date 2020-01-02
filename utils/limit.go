package utils

import (
  "fmt"
  "errors"
  "os/exec"
)

const limitBinPath = "__main__/utils/limit"
const oom = "Fatal error: out of memory."

var ErrOutOfMemory = errors.New("Out of memory")

func RunWithMemLimit(cmd *exec.Cmd, limitBytes int) error {
  cmd2 := *cmd
  cmd2.Path = Runfile(limitBinPath)
  cmd2.Args = append([]string{cmd2.Path,fmt.Sprintf("%d",limitBytes)},cmd.Args...)
  return (&cmd2).Run()
}
