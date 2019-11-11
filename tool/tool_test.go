package tool

import (
  "context"
  "testing"
  "log"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/problems"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

func TestTptpToProto(t *testing.T) {
  for k,v := range problems.SampleProblems {
    _,err := TptpToProto(context.Background(),FOF,v)
    if err!=nil { t.Errorf("TptpToProto(%q): %v",k,err) }
  }
}

func TestProtoToTptpt(t *testing.T) {
  ctx := context.Background()
  for k,v := range problems.SampleProblems {
    fof,err := TptpToProto(ctx,FOF,v)
    if err!=nil { t.Fatalf("TptpToProto(%q[1]): %v",k,err) }
    tptp,err := ProtoToTptp(ctx,fof)
    if err!=nil { t.Fatalf("ProtoToTptp(%q): %v",k,err) }
    fof2,err := TptpToProto(ctx,FOF,tptp)
    if err!=nil { t.Fatalf("TptpToProto(%q[2]): %v",k,err) }
    if !proto.Equal(fof,fof2) {
      t.Errorf("TptpToProto;ProtoToTptp;TptpToProto(%q) = %v, want %v",k,fof2,fof)
    }
  }
}

func TestFOFToCNF(t *testing.T) {
  for k,v := range problems.SampleProblems {
    log.Printf("%s",k)
    log.Printf("tptp -> fof")
    fof,err := TptpToProto(context.Background(),FOF,v)
    if err!=nil { t.Fatalf("TptpToProto(%q): %v",k,err) }
    log.Printf("fof -> cnf")
    cnf,err := FOFToCNF(context.Background(),fof)
    if err!=nil { t.Fatalf("FOFToCNF(%q): %v",k,err) }
    log.Printf("iterating over inputs")
    for _,i := range cnf.Input {
      if got,want := i.Language,tpb.Input_CNF; got!=want {
        t.Errorf("i.Language = %v, want %v",got,want)
      }
    }
  }
}

func pred(name string) *tpb.Formula {
  return &tpb.Formula{
    Formula: &tpb.Formula_Pred_{
      Pred: &tpb.Formula_Pred {
        Type: tpb.Formula_Pred_CUSTOM,
        Name: name,
      },
    },
  }
}

func op(t tpb.Formula_Operator_Type, args []*tpb.Formula) *tpb.Formula {
  return &tpb.Formula {
    Formula: &tpb.Formula_Op {
      Op: &tpb.Formula_Operator { Type: t, Args: args },
    },
  }
}

func neg(f *tpb.Formula) *tpb.Formula {
  return op(tpb.Formula_Operator_NEG, []*tpb.Formula { f })
}

func or(disjuncts... *tpb.Formula) *tpb.Formula {
  return op(tpb.Formula_Operator_OR, disjuncts)
}

func TestValidateProofOK(t *testing.T) {
  p := pred("p")
  clauses := []*tpb.Input{
    {
      Language: tpb.Input_CNF,
      Role: tpb.Input_PLAIN,
      Formula: p,
    },
    {
      Language: tpb.Input_CNF,
      Role: tpb.Input_PLAIN,
      Formula: neg(p),
    },
  }
  cnfProblem := &tpb.File {
    Input: clauses,
  }
  cnfProof := &tpb.File {
    Input: clauses,
  }
  if _,err := ValidateProof(context.Background(),&spb.CNF{Problem:cnfProblem,Proof:cnfProof}); err!=nil {
    t.Errorf("ValidateProof(): %v",err)
  }
}

func TestValidateProofFail(t *testing.T) {
  cnfProblem := &tpb.File {
    Input: []*tpb.Input{
      {
        Language: tpb.Input_CNF,
        Role: tpb.Input_PLAIN,
        Formula: &tpb.Formula{
          Formula: &tpb.Formula_Pred_{
            Pred: &tpb.Formula_Pred {
              Type: tpb.Formula_Pred_CUSTOM,
              Name: "p",
            },
          },
        },
      },
    },
  }
  cnfProof := &tpb.File {
    Input: []*tpb.Input{},
  }
  if stats,err := ValidateProof(context.Background(),&spb.CNF{Problem:cnfProblem,Proof:cnfProof}); err==nil {
    t.Errorf("ValidateProof() = %v, want error",stats)
  }
}
