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
  if _,err := ValidateProof(context.Background(),cnfProblem,cnfProof); err!=nil {
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
  if stats,err := ValidateProof(context.Background(),cnfProblem,cnfProof); err==nil {
    t.Errorf("ValidateProof() = %v, want error",stats)
  }
}
