package xgbtrain_test

import (
  "bytes"
  "context"
  "fmt"
  "time"
  "testing"
  "os"
  "os/exec"

  "google.golang.org/protobuf/types/known/durationpb"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/problems/sample"
  "github.com/pompon0/tptp_benchmark_go/ffprover"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"

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

func XGBTrain(ctx context.Context, data *mpb.Data) (*mpb.Model,error) {
  cmd := exec.CommandContext(ctx,utils.Runfile(xgbtrain_bin_path),
    fmt.Sprintf("--features_space_size=%v",data.GetFeaturesSpaceSize()),
  )
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.WriteString(data.GetLibsvm()); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %w",err)
  }
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err:=cmd.Run(); err!=nil {
    return nil,fmt.Errorf("cmd.Run(): %w",err)
  }
  return &mpb.Model{Xgb: outBuf.Bytes()},nil
}

func TestXgbTrain(t *testing.T) {
  ctx := context.Background()
  name := "simple"
  tptpFOF,ok := sample.SampleProblems()[name]
  if !ok { t.Fatalf("problem %q is missing") }

  tptpCNF,err := eprover.FOFToCNF(ctx,tptpFOF)
  if err!=nil { t.Fatalf("eprover.FOFToCNF(): %v",err) }

  cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
  if err!=nil { t.Fatalf("tool.TptpToProto(): %v",err) }

  input := &mpb.Input{
    Problem: cnf,
    Timeout: durationpb.New(time.Minute),
    FeaturesSpaceSize: 1<<15,
  }

  // without a model
  out,err := mcts.MCTS(ctx,input)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }
  fmt.Printf("out = %v",out)

  // train a models
  input.Priority,err = XGBTrain(ctx,out.GetPriority())
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }
  input.Reward,err = XGBTrain(ctx,out.GetReward())
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }

  // with model
  _,err = mcts.MCTS(ctx,input)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }
}
