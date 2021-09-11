package xgbtrain

import (
  "context"
  "fmt"
  "time"
  "testing"

  "google.golang.org/protobuf/types/known/durationpb"

  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/problems/sample"
  "github.com/pompon0/tptp_benchmark_go/ffprover"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
)


func TestXGBTrain(t *testing.T) {
  ctx := context.Background()
  name := "l143_zfmisc_1"
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
    PlayoutsPerBigstep: 100,
    PlayoutDepth: 20,
  }

  // without a model
  out,err := mcts.MCTS(ctx,input)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }
  fmt.Printf("out = %v",out)

  data := NewData()
  data.Append(out.GetPath())

  // train a models
  input.Models = &mpb.ModelSet{}
  input.Models.Priority,err = XGBTrain(ctx,data.Priority)
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }
  input.Models.Reward,err = XGBTrain(ctx,data.Reward)
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }

  // with model
  _,err = mcts.MCTS(ctx,input)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }
}
