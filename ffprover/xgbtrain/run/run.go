package main

import (
  "context"
  "fmt"
  "flag"
  "log"
  "time"
  "io/ioutil"
  "archive/zip"
  "strings"

  "github.com/golang/protobuf/proto"
  "google.golang.org/protobuf/types/known/durationpb"
  mcts "github.com/pompon0/tptp_benchmark_go/ffprover"
  "github.com/pompon0/tptp_benchmark_go/ffprover/xgbtrain"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
)

var outputZipPath = flag.String("output_zip_path","","")
var modelsPath = flag.String("models_path","","")

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
  byStatus := map[mpb.Status]int{}
  data := xgbtrain.NewData()
  for _,out := range outs {
    data.Append(out.GetPath())
    byStatus[out.GetStatus()]++
  }
  fmt.Printf("byStatus = %+v\n",byStatus)
  if models.Priority,err = xgbtrain.XGBTrain(ctx,data.Priority); err!=nil {
    return fmt.Errorf("xgbtrain.XGBTrain(): %w",err)
  }
  if models.Reward,err = xgbtrain.XGBTrain(ctx,data.Reward); err!=nil {
    return fmt.Errorf("xgbtrain.XGBTrain(): %w",err)
  }
  modelsBytes,err := proto.Marshal(models)
  if err!=nil {
    return fmt.Errorf("proto.Marshal(): %w",err)
  }
  if err:=ioutil.WriteFile(*modelsPath,modelsBytes,0777); err!=nil {
    return fmt.Errorf("ioutil.WriteFile(): %w",err)
  }

  for _,out := range outs {
    if out.GetStatus()!=mpb.Status_THEOREM { continue }
    name := out.GetProblem().GetName()
    // TODO: this might need to be replaced by a more specialized tooling for efficiency:
    // a binary which takes a set of problems and expected output paths, which emulates MCTS executing the model.
    // It is nice, because it is as if we were providing a precomputed MCTS subtree.
    res,err := mcts.MCTS(ctx,&mpb.Input{
      Problem: out.GetProblem(),
      Timeout: durationpb.New(10*time.Second), // TODO: make it minimal, so that it still works
      // TODO: put it into mpb.Model, so that we can cross check early the feature space compatibility
      // In fact we should even make a FeaturesSpace descriptor to early identify more implementation problems than just size inconsistency.
      FeaturesSpaceSize: data.Reward.FeaturesSpaceSize,
      Models: models,
      PlayoutsPerBigstep: 0,
      PlayoutDepth: 0, // not important, given that we don't allow playouts.
    })
    if err!=nil {
      return fmt.Errorf("mcts.MCTS(%q): %w",name,err)
    }
    if got,want:=res.GetStatus(),mpb.Status_THEOREM; got!=want {
      return fmt.Errorf("mcts.MCTS(%q).status = %v, want %v",name,got,want)
    }
    log.Printf("mcts.MCTS(%q): OK",name)
  }

  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
