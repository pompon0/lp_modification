package xgbtrain

import (
  "context"
  "fmt"
  "time"
  "testing"

  "google.golang.org/protobuf/types/known/durationpb"

  "github.com/golang/protobuf/proto"
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

  cnf,err := tptpCNF.ToProto(ctx,tool.CNF)
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

  if got,want := out.GetStatus(),mpb.Status_THEOREM; got!=want {
    t.Fatalf("MCTS(%q)[no model] = %v, want %v",name,got,want)
  }

  data := NewData()
  data.Append(out.GetPath())

  // train a models
  input.Models = &mpb.ModelSet{}
  input.Models.Priority,err = XGBTrain(ctx,data.Priority)
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }
  input.Models.Reward,err = XGBTrain(ctx,data.Reward)
  if err!=nil { t.Fatalf("XGBTrain(): %v",err) }

  // with model
  input.Problem = out.Problem // problem should be preserved by MCTS.
  input.PlayoutsPerBigstep = 0 // model should be overfitted
  outWithModel,err := mcts.MCTS(ctx,input)
  if err!=nil { t.Fatalf("MCTS(): %v",err) }

  if got,want := outWithModel.GetStatus(),mpb.Status_THEOREM; got!=want {
    t.Fatalf("MCTS(%q)[with model] = %v, want %v",name,got,want)
  }

  normalizePath := func(p *mpb.Path) *mpb.Path {
    p2 := &mpb.Path{Won: p.GetWon()}
    for _,node := range p.GetNodes() {
      p2.Nodes = append(p2.Nodes,&mpb.Path_Node{
        Actions: make([]*mpb.Path_Instance,len(node.GetActions())),
        ChosenAction: node.GetChosenAction(),
      })
    }
    return p2
  }

  // TODO: upgrade this test so that the result is not trivial.
  /* trivialPath := true
  for _,node := range out.GetPath().GetNodes() {
    trivialPath = trivialPath && (node.GetChosenAction()==0)
  }
  if trivialPath {
    t.Errorf("result path is trivial, model test is useless")
  }*/
  p1 := normalizePath(out.GetPath())
  p2 := normalizePath(outWithModel.GetPath())
  if !proto.Equal(p1,p2) {
    t.Errorf("outWithModel.path = %v, want %v",p2,p1)
  }
}
