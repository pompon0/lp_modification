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
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
