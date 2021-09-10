package main

import (
  "context"
  "fmt"
  "flag"
  "log"
  "io/ioutil"
  "archive/zip"
  "strings"

  "github.com/golang/protobuf/proto"

  "github.com/pompon0/tptp_benchmark_go/ffprover/xgbtrain"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
)

var outputZipPath = flag.String("output_zip_path","","")
var modelsPath = flag.String("model_path","","")

// zip format is preferred over tar.gz, because it provides random access to files.
// if zip (per-file) compression is insufficient, consider switching to LZMA.
func readZip() (map[string]*mpb.Output,error) {
  r,err := zip.OpenReader(*outputZipPath)
  if err != nil { return nil,fmt.Errorf("zip.OpenReader(): %w",err) }
  defer r.Close()
  outs := map[string]*mpb.Output{}
  for _,f := range r.File {
    if strings.HasSuffix(f.Name,"/") {
      continue
    }
    // f is not a directory
    r,err := f.Open()
    if err!=nil { return nil,fmt.Errorf("f.Open(): %w",err) }
    defer r.Close()
    outBytes,err := ioutil.ReadAll(r)
    if err!=nil { return nil,fmt.Errorf("ioutil.ReadlAll(): %w",err) }
    var outProto mpb.Output
    if err:=proto.Unmarshal(outBytes,&outProto); err!=nil {
      return nil,fmt.Errorf("proto.Unmarshal(): %w",err)
    }
    outs[f.Name] = &outProto
  }
  return outs,nil
}

func run(ctx context.Context) error {
  outs,err := readZip()
  if err!=nil { return fmt.Errorf("readZip(): %w",err) }
  models := &mpb.ModelSet{}
  priority := &mpb.LibSVM{}
  reward := &mpb.LibSVM{}
  for _,out := range outs {
    //TODO: increase weight of positive samples
    priority.Instances = append(priority.GetInstances(),out.GetPriority().GetInstances()...)
    reward.Instances = append(reward.GetInstances(),out.GetReward().GetInstances()...)
  }
  if models.Priority,err = xgbtrain.XGBTrain(ctx,priority); err!=nil {
    return fmt.Errorf("xgbtrain.XGBTrain(): %w",err)
  }
  if models.Reward,err = xgbtrain.XGBTrain(ctx,reward); err!=nil {
    return fmt.Errorf("xgbtrain.XGBTrain(): %w",err)
  }
  modelsBytes,err := proto.Marshal(models)
  if err!=nil {
    return fmt.Errorf("proto.Marshal(): %w",err)
  }
  if err:=ioutil.WriteFile(*modelsPath,modelsBytes,0777); err!=nil {
    return fmt.Errorf("ioutil.WriteFile(): %w",err)
  }
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %b",err)
  }
}
