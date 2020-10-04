package utils

import (
  "context"
  "fmt"
  "errors"
  "syscall"
  "os/exec"
)

const limitBinPath = "__main__/utils/limit"
const oom = "Fatal error: out of memory."

var ErrOutOfMemory = errors.New("Out of memory")

func RunWithMemLimit(ctx context.Context, cmd *exec.Cmd, limitBytes int) error {
  cmd2 := *cmd
  cmd2.Path = Runfile(limitBinPath)
  cmd2.Args = append([]string{cmd2.Path,fmt.Sprintf("%d",limitBytes)},cmd.Args...)
  // cmd.Run doesn't kill subprocesses, we need to do it on our own.
  // Make cmd.Run() execute subprocess in a new process group.
  cmd2.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
  ctx,cancel := context.WithCancel(ctx)
  defer cancel()
  // Start the process before starting the killer goroutine,
  // so that we are guaranteed that cmd2.Process has been populated.
  if err:=(&cmd2).Start(); err!=nil {
    return err
  }
  go func(){
    <-ctx.Done()
    syscall.Kill(-cmd2.Process.Pid, syscall.SIGKILL)
  }()
  return (&cmd2).Wait()
}
