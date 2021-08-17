package mcts

import (
  "context"
  "log"
  "strconv"
  "fmt"
  "flag"
  "bytes"
  "time"
  "os"
  "sort"
  "strings"
  "path"
  "os/exec"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/utils"
)

const mcts_bin_path = "__main__/ffprover/mcts"

func MCTS(ctx context.Context, problemName string, tptpFOFProblem []byte, out Output) error {
  tptpPath,cleanup,err := tool.WriteTmp(tptpFOFProblem)
  if err!=nil { return fmt.Errorf("tool.WriteTmp(): %w",err) }
  defer cleanup()

  cmd := exec.Command(utils.Runfile(mcts_bin_path),
    fmt.Sprintf("--timeout=%v",*timeout),
    fmt.Sprintf("--problem_path=%v",tptpPath),
    fmt.Sprintf("--priority_model_path=%v",*priorityModelPath),
    fmt.Sprintf("--reward_model_path=%v",*rewardModelPath),
    fmt.Sprintf("--priority_training_path=%v",out.PriorityDir.File(problemName)),
    fmt.Sprintf("--reward_training_path=%v",out.RewardDir.File(problemName)),
    fmt.Sprintf("--full_search=%v",*fullSearch),
  )
  var inBuf,outBuf bytes.Buffer
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  const memLimitBytes = 2*1000*1000*1000
  gracefulExitTimeout := time.Minute
  cmdCtx,cancel := context.WithTimeout(ctx,*timeout+gracefulExitTimeout)
  defer cancel()
  if err := utils.RunWithMemLimit(cmdCtx,cmd,memLimitBytes); err!=nil {
    return fmt.Errorf("cmd.Run(): %w",err)
  }
  return nil
}
