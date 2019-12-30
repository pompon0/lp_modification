package tool

import (
  "context"
  "testing"
  "log"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/problems"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const num845_2 = `
fof('qu(ind(267), imp(267))',conjecture,( ! [Vd416] : ( vmul(vsucc(vd411),Vd416) = vplus(vmul(vd411,Vd416),Vd416) => vmul(vsucc(vd411),vsucc(Vd416)) = vplus(vmul(vd411,vsucc(Vd416)),vsucc(Vd416)) ) )).
fof('ass(cond(conseq(263), 1), 0)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vplus(vmul(vd411,Vd413),vd411),vsucc(Vd413)) = vplus(vmul(vd411,vsucc(Vd413)),vsucc(Vd413)) ) )).
fof('ass(cond(conseq(263), 1), 1)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vmul(vd411,Vd413),vplus(vd411,vsucc(Vd413))) = vplus(vplus(vmul(vd411,Vd413),vd411),vsucc(Vd413)) ) )).
fof('ass(cond(conseq(263), 1), 2)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vmul(vd411,Vd413),vsucc(vplus(vd411,Vd413))) = vplus(vmul(vd411,Vd413),vplus(vd411,vsucc(Vd413))) ) )).
fof('ass(cond(conseq(263), 1), 3)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vmul(vd411,Vd413),vplus(vsucc(vd411),Vd413)) = vplus(vmul(vd411,Vd413),vsucc(vplus(vd411,Vd413))) ) )).
fof('ass(cond(conseq(263), 1), 4)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vmul(vd411,Vd413),vplus(Vd413,vsucc(vd411))) = vplus(vmul(vd411,Vd413),vplus(vsucc(vd411),Vd413)) ) )).
fof('ass(cond(conseq(263), 1), 5)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vplus(vmul(vd411,Vd413),Vd413),vsucc(vd411)) = vplus(vmul(vd411,Vd413),vplus(Vd413,vsucc(vd411))) ) )).
fof('ass(cond(conseq(263), 1), 6)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vplus(vmul(vsucc(vd411),Vd413),vsucc(vd411)) = vplus(vplus(vmul(vd411,Vd413),Vd413),vsucc(vd411)) ) )).
fof('ass(cond(conseq(263), 1), 7)',axiom,( ! [Vd413] : ( vmul(vsucc(vd411),Vd413) = vplus(vmul(vd411,Vd413),Vd413) => vmul(vsucc(vd411),vsucc(Vd413)) = vplus(vmul(vsucc(vd411),Vd413),vsucc(vd411)) ) )).
fof('holds(264, 412, 2)',axiom,( vsucc(vmul(vd411,v1)) = vplus(vmul(vd411,v1),v1) )).
fof('qu(cond(conseq(axiom(3)), 32), and(holds(definiens(249), 399, 0), holds(definiens(249), 398, 0)))',axiom,( ! [Vd396] : ! [Vd397] : ( vmul(Vd396,vsucc(Vd397)) = vplus(vmul(Vd396,Vd397),Vd396) & vmul(Vd396,v1) = Vd396 ) )).
fof('ass(cond(61, 0), 0)',axiom,( ! [Vd78] : ! [Vd79] : vplus(Vd79,Vd78) = vplus(Vd78,Vd79) )).
fof('ass(cond(43, 0), 0)',axiom,( ! [Vd59] : vplus(v1,Vd59) = vsucc(Vd59) )).
fof('ass(cond(33, 0), 0)',axiom,( ! [Vd46] : ! [Vd47] : ! [Vd48] : vplus(vplus(Vd46,Vd47),Vd48) = vplus(Vd46,vplus(Vd47,Vd48)) )).
fof('qu(cond(conseq(axiom(3)), 3), and(holds(definiens(29), 45, 0), holds(definiens(29), 44, 0)))',axiom,( ! [Vd42] : ! [Vd43] : ( vplus(Vd42,vsucc(Vd43)) = vsucc(vplus(Vd42,Vd43)) & vplus(Vd42,v1) = vsucc(Vd42) ) )).
`

func parsingTestCases() map[string][]byte {
  r := map[string][]byte{}
  for k,v := range problems.SampleProblems { r[k] = v}
  r["NUM845+2.p"] = []byte(num845_2)
  return r
}

func TestTptpToProto(t *testing.T) {
  for k,v := range parsingTestCases() {
    _,err := TptpToProto(context.Background(),FOF,v)
    if err!=nil { t.Errorf("TptpToProto(%q): %v",k,err) }
  }
}


func TestProtoToTptp(t *testing.T) {
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

func TestProtoFOFToCNF(t *testing.T) {
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

func TestTPTPFOFToCNF(t *testing.T) {
  for k,v := range problems.SampleProblems {
    log.Printf("%s",k)
    cnf,err := eprover.FOFToCNF(context.Background(),v)
    if err!=nil { t.Fatalf("FOFToCNF(%q): %v",k,err) }
    log.Printf("cnf = %s",string(cnf))
    cnfProto,err := TptpToProto(context.Background(),CNF,cnf)
    if err!=nil { t.Fatalf("TptpToProto(%q): %v",k,err) }
    for _,i := range cnfProto.Input {
      if got,want := i.Language,tpb.Input_CNF; got!=want {
        t.Errorf("i.Language = %v, want %v",got,want)
      }
    }
  }
}

func TestTptpHasEquality(t *testing.T) {
  ctx := context.Background()
  for _,c := range []struct { name string; tptp []byte; want bool } {
    {"trivial", problems.Trivial, false},
    {"simple", problems.Simple, false},
    {"eqAxiom1", problems.EqAxiom1, true},
    {"eqAxiom2", problems.EqAxiom2, true},
    {"eqAxiom3", problems.EqAxiom3, true},
    {"barber", problems.Barber, false},
    {"pelletier20", problems.Pelletier20, false},
    {"pelletier24", problems.Pelletier24, false},
  } {
    got,err := TptpHasEquality(ctx,c.tptp)
    if err!=nil {
      t.Errorf("TptpHasEquality(%q): %v",c.name,err)
    } else if got!=c.want {
      t.Errorf("TptpHasEquality(%q) = %, want %",got,c.want)
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
