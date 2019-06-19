package tool

import (
  "context"
  "testing"

  "github.com/pompon0/tptp_benchmark_go/problems"
  tpb "github.com/pompon0/tptp_parser/proto/tptp_go_proto"
)

func TestTptpToProto(t *testing.T) {
  for k,v := range problems.SampleProblems {
    _,err := TptpToProto(context.Background(),FOF,v)
    if err!=nil { t.Errorf("TptpToProto(%q): %v",k,err) }
  }
}

func TestFOFToCNF(t *testing.T) {
  for k,v := range problems.SampleProblems {
    fof,err := TptpToProto(context.Background(),FOF,v)
    if err!=nil { t.Fatalf("TptpToProto(%q): %v",k,err) }
    cnf,err := FOFToCNF(context.Background(),fof)
    if err!=nil { t.Errorf("FOFToCNF(%q): %v",k,err) }
    for _,i := range cnf.Input {
      if got,want := i.Language,tpb.Input_CNF; got!=want {
        t.Errorf("i.Language = %v, want %v",got,want)
      }
    }
  }
}
