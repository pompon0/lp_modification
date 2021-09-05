package main

import (
  "context"
  "fmt"
  "flag"
  "log"
  "io/ioutil"

  "github.com/golang/protobuf/proto"

  "github.com/pompon0/tptp_benchmark_go/ffprover/xgbtrain"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
)

var outputPath = flag.String("output_path","","")
var modelsPath = flag.String("model_dir_path","","")

func run(ctx context.Context) error {
  outBytes,err := ioutil.ReadFile(*outputPath)
  if err!=nil { return fmt.Errorf("ioutil.ReadFile(): %w",err) }
  var outProto mpb.Output
  if err:=proto.Unmarshal(outBytes,&outProto); err!=nil {
    return fmt.Errorf("proto.Unmarshal(): %w",err)
  }
  models := &mpb.ModelSet{}
  if models.Priority,err = xgbtrain.XGBTrain(ctx,outProto.GetPriority()); err!=nil {
    return fmt.Errorf("xgbtrain.XGBTrain(): %w",err)
  }
  if models.Reward,err = xgbtrain.XGBTrain(ctx,outProto.GetReward()); err!=nil {
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
