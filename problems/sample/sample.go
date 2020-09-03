package sample

import (
  "log"
  "io/ioutil"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/utils"
  spb "github.com/pompon0/tptp_benchmark_go/problems/sample/sample_go_proto"
)

const sampleProblemsPath = "__main__/problems/sample/sample.textpb"

func SampleProblems() map[string][]byte {
  raw,err := ioutil.ReadFile(utils.Runfile(sampleProblemsPath))
  if err!=nil {
    log.Fatalf("ioutils.ReadFile(): %v",err)
  }
  var problemSet spb.Set
  if err:=proto.UnmarshalText(string(raw),&problemSet); err!=nil {
    log.Fatalf("proto.UnmarshalText(): %v",err)
  }
  problems := map[string][]byte{}
  for _,p := range problemSet.GetProblems() {
    problems[p.GetName()] = []byte(p.GetTptp())
  }
  return problems
}
