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

func printFeature(f *mpb.LibSVM_Feature) string {
  return fmt.Sprintf("%d:%f",f.GetIndex(),f.GetValue())
}

func printInstance(i *mpb.LibSVM_Instance) string {
  var feat []string
  for _,f := range i.GetFeatures() {
    feat = append(feat,printFeature(f))
  }
  return fmt.Sprintf("%f %s\n",i.GetLabel(),strings.Join(feat," "))
}

func printLibSVM(data *mpb.LibSVM) string {
  var inst []string
  for _,i := range data.GetInstances() {
    inst = append(inst,printInstance(i))
  }
  return strings.Join(inst,"")
}

const xgbtrain_bin_path = "__main__/ffprover/xgbtrain/xgb-train"

func XGBTrain(ctx context.Context, data *mpb.LibSVM) (*mpb.Model,error) {
  cmd := exec.CommandContext(ctx,utils.Runfile(xgbtrain_bin_path),
    fmt.Sprintf("--features_space_size=%v",data.GetFeaturesSpaceSize()),
  )
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.WriteString(printLibSVM(data)); err!=nil {
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
