package xgbtrain_test

import (
  "bytes"
  "context"
  "fmt"
  "time"
  "testing"
  "os"
  "os/exec"
  "io/ioutil"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/problems/sample"
)

type Model struct {
  priority []byte
  reward []byte
}

type Output struct {
  priority []byte
  reward []byte
}

const mcts_bin_path = "__main__/ffprover/mcts"
const xgbtrain_bin_path = "__main__/ffprover/xgbtrain/xgb-train"

func XGBTrain(ctx context.Context, output []byte) ([]byte,error) {
  outputPath,cleanup,err := tool.WriteTmp(output)
  if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
  defer cleanup()

  modelPath,cleanup,err := tool.WriteTmp(nil)
  if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
  defer cleanup()

  cmd := exec.CommandContext(ctx,utils.Runfile(xgbtrain_bin_path),outputPath,modelPath)
  var inBuf bytes.Buffer
  cmd.Stdin = &inBuf
  cmd.Stdout = os.Stderr
  cmd.Stderr = os.Stderr
  if err:=cmd.Run(); err!=nil {
    return nil,fmt.Errorf("cmd.Run(): %w",err)
  }

  model,err := ioutil.ReadFile(modelPath)
  if err!=nil { return nil,fmt.Errorf("ioutil.Read(): %w",err) }
  return model,nil
}

// model can be nil
func MCTS(ctx context.Context, tptpFOFProblem []byte, model *Model) (*Output,error) {
  tptpPath,cleanup,err := tool.WriteTmp(tptpFOFProblem)
  if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
  //defer cleanup()

  priorityModelPath := ""
  rewardModelPath := ""

  if model!=nil {
    priorityModelPath,cleanup,err = tool.WriteTmp(model.priority)
    if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
    //defer cleanup()

    rewardModelPath,cleanup,err = tool.WriteTmp(model.reward)
    if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
    //defer cleanup()
  }

  priorityOutputPath,cleanup,err := tool.WriteTmp(nil)
  if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
  defer cleanup()

  rewardOutputPath,cleanup,err := tool.WriteTmp(nil)
  if err!=nil { return nil,fmt.Errorf("tool.WriteTmp(): %w",err) }
  defer cleanup()

  timeout := time.Minute
  cmd := exec.Command(utils.Runfile(mcts_bin_path),
    fmt.Sprintf("--timeout=%v",timeout),
    fmt.Sprintf("--problem_path=%v",tptpPath),
    fmt.Sprintf("--priority_model_path=%v",priorityModelPath),
    fmt.Sprintf("--reward_model_path=%v",rewardModelPath),
    fmt.Sprintf("--priority_training_path=%v",priorityOutputPath),
    fmt.Sprintf("--reward_training_path=%v",rewardOutputPath),
  )
  var inBuf,outBuf bytes.Buffer
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  const memLimitBytes = 2*1000*1000*1000
  cmdCtx,cancel := context.WithTimeout(ctx,timeout)
  defer cancel()
  if err := utils.RunWithMemLimit(cmdCtx,cmd,memLimitBytes); err!=nil {
    return nil,fmt.Errorf("cmd.Run(): %w",err)
  }
  out := &Output{}
  out.priority,err = ioutil.ReadFile(priorityOutputPath)
  if err!=nil { return nil,fmt.Errorf("ioutil.ReadFile(): %v") }
  out.reward,err = ioutil.ReadFile(rewardOutputPath)
  if err!=nil { return nil,fmt.Errorf("ioutil.ReadFile(): %v") }
  return out,nil
}

func TestXgbTrain(t *testing.T) {
  ctx := context.Background()
  name := "simple"
  tptpFOFProblem,ok := sample.SampleProblems()[name]
  if !ok { t.Fatalf("problem %q is missing") }

  // without a model
  out,err := MCTS(ctx,tptpFOFProblem,nil)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }
  fmt.Printf("out = %v",out)

  // train a models
  model := &Model{}
  model.priority,err = XGBTrain(ctx,out.priority)
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }
  model.reward,err = XGBTrain(ctx,out.reward)
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }

  // with model
  _,err = MCTS(ctx,tptpFOFProblem,model)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }
}
