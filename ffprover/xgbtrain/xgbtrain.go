package xgbtrain

import (
  "bytes"
  "context"
  "fmt"
  "strings"
  "os"
  "os/exec"

  "github.com/pompon0/tptp_benchmark_go/utils"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
)

func printFeature(f *mpb.Path_Feature) string {
  return fmt.Sprintf("%d:%f",f.GetIndex(),f.GetValue())
}

func printInstance(i *mpb.Path_Instance) string {
  var feat []string
  for _,f := range i.GetFeatures() {
    feat = append(feat,printFeature(f))
  }
  return fmt.Sprintf("%f %s\n",i.GetLabel(),strings.Join(feat," "))
}

type LibSVM struct {
  Instances []*mpb.Path_Instance
  FeaturesSpaceSize uint64
}

func (l *LibSVM) String() string {
  var inst []string
  for _,i := range l.Instances {
    inst = append(inst,printInstance(i))
  }
  return strings.Join(inst,"")
}

type Data struct {
  Priority *LibSVM
  Reward *LibSVM
}

func NewData() *Data {
  return &Data {
    Priority: &LibSVM{},
    Reward: &LibSVM{},
  }
}

func (d *Data) Append(path *mpb.Path) {
  //TODO: increase weight of positive samples
  for _,node := range path.GetNodes() {
    d.Reward.Instances = append(d.Reward.Instances,node.GetState())
    if path.GetWon() {
      for _,action := range node.GetActions() {
        d.Priority.Instances = append(d.Priority.Instances,action)
      }
    }
  }
  d.Reward.FeaturesSpaceSize = path.GetFeaturesSpaceSize()
  d.Priority.FeaturesSpaceSize = path.GetFeaturesSpaceSize()
}

const xgbtrain_bin_path = "__main__/ffprover/xgbtrain/xgb-train"

func XGBTrain(ctx context.Context, data *LibSVM) (*mpb.Model,error) {
  cmd := exec.CommandContext(ctx,utils.Runfile(xgbtrain_bin_path),
    fmt.Sprintf("--features_space_size=%v",data.FeaturesSpaceSize),
  )
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.WriteString(data.String()); err!=nil {
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
