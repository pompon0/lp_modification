package tool

import (
  "context"
  "testing"
  "log"
  "bytes"

  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/problems/sample"
  "github.com/google/go-cmp/cmp"
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
  for k,v := range sample.SampleProblems() { r[k] = v }
  r["NUM845+2.p"] = []byte(num845_2)
  return r
}

func TestClearUnknownNames(t *testing.T) {
  ctx := context.Background()
  fof := sample.SampleProblems()["barber"]
  cnf,err := eprover.FOFToCNF(ctx,fof)
  if err!=nil { t.Fatalf("FOFToCNF(): %v",err) }
  fofProto,err := TptpToProto(ctx,FOF,fof)
  if err!=nil { t.Fatalf("TptpToProto(FOF): %v",err) }
  cnfProto,err := TptpToProto(ctx,CNF,cnf)
  if err!=nil { t.Fatalf("TptpToProto(CNF): %v",err) }

  getNames := func(f *tpb.File) map[string]bool {
    names := map[string]bool{}
    for _,node := range f.GetNodes() {
      if n := node.GetName(); n!="" {
        names[n] = true
      }
    }
    return names
  }

  fofNames := getNames(fofProto)
  cnfNames := getNames(cnfProto)
  // Function under test:
  ClearUnknownNames(cnfProto,fofProto.GetNodes())
  cnfNames2 := getNames(cnfProto)

  for k,_ := range fofNames {
    if _,ok := cnfNames[k]; !ok {
      t.Fatalf("test case not valid: cnfNames = %v, want strict superset of %v",cnfNames,fofNames)
    }
  }
  if cmp.Equal(fofNames,cnfNames) {
    t.Fatalf("test case useless: conversion to CNF didn't introduce new names")
  }
  // Correctness check:
  if !cmp.Equal(fofNames,cnfNames2) {
    t.Fatalf("cnfNames2 = %v, want %v",cnfNames2,fofNames)
  }
}

func TestReplaceEquality(t *testing.T) {
  ctx := context.Background()
  for k,v := range sample.SampleProblems() {
    fof,err := TptpToProto(ctx,FOF,v)
    if err!=nil { t.Fatalf("TptpToProto(%q): %v",k,err) }
    // under test
    fofNoEq := ReplaceEquality(fof)

    tptpNoEq,err := ProtoToTptp(ctx,fofNoEq)
    if err!=nil { t.Fatalf("ProtoToTptp(%q): %v",k,err) }

    before,err := TptpHasEquality(ctx,v)
    if err!=nil { t.Fatalf("TptpHasEquality(%q): %v",k,err) }
    after,err := TptpHasEquality(ctx,tptpNoEq)
    if err!=nil { t.Fatalf("TptpHasEquality(%q): %v",k,err) }

    if after {
      t.Errorf("tptpNoEq[%q] still has equality predicate = %q",k,tptpNoEq)
    }

    if !before {
      tptp,err := ProtoToTptp(ctx,fof)
      if err!=nil { t.Fatalf("ProtoToTptp(%q): %v",k,err) }
      if !bytes.Equal(tptpNoEq,tptp) {
        t.Errorf("tptpNoEq[%q] = %q, want %q",k,tptpNoEq,tptp)
      }
    }
  }
}

func TestTptpToProto(t *testing.T) {
  for k,v := range parsingTestCases() {
    _,err := TptpToProto(context.Background(),FOF,v)
    if err!=nil { t.Errorf("TptpToProto(%q): %v",k,err) }
  }
}


func TestProtoToTptp(t *testing.T) {
  ctx := context.Background()
  for k,v := range sample.SampleProblems() {
    fof,err := TptpToProto(ctx,FOF,v)
    if err!=nil { t.Fatalf("TptpToProto(%q[1]): %v",k,err) }
    tptp,err := ProtoToTptp(ctx,fof)
    if err!=nil { t.Fatalf("ProtoToTptp(%q[1]): %v",k,err) }
    fof2,err := TptpToProto(ctx,FOF,tptp)
    if err!=nil { t.Fatalf("TptpToProto(%q[2]): %v",k,err) }
    tptp2,err := ProtoToTptp(ctx,fof2)
    if err!=nil { t.Fatalf("ProtoToTptp(%q[2]): %v",k,err) }
    if !bytes.Equal(tptp,tptp2) {
      t.Errorf("(TptpToProto;ProtoToTptp)^2(%q) = %v, want %v, initial %v",k,string(tptp2),string(tptp),string(v))
    }
  }
}

func TestProofToTptp(t *testing.T) {
  clause := &tpb.Input {
    Name: "a0",
    Language: tpb.Input_CNF,
    Role: tpb.Input_PLAIN,
    Formula: []int32{0,0},
  }
  proof := &spb.Proof {
    Clauses: []*spb.Derivation{{
      Derived: clause,
      Sources: []*spb.Source{{
        Ground: clause,
        Source: clause,
      }},
    }},
    Nodes: []*tpb.Node{{Id: 0, Type: tpb.Type_FORM_OR}},
  }
  if _,err := ProofToTptp(context.Background(),proof); err!=nil {
    t.Errorf("ProofToTptp(): %v",err)
  }
}

func TestTPTPFOFToCNF(t *testing.T) {
  ctx := context.Background()
  for k,v := range sample.SampleProblems() {
    log.Printf("%s",k)
    cnf,err := eprover.FOFToCNF(ctx,v)
    if err!=nil { t.Fatalf("FOFToCNF(%q): %v",k,err) }
    log.Printf("cnf = %s",string(cnf))
    cnfProto,err := TptpToProto(ctx,CNF,cnf)
    if err!=nil { t.Fatalf("TptpToProto(%q): %v",k,err) }
    for _,i := range cnfProto.Input {
      if got,want := i.Language,tpb.Input_CNF; got!=want {
        t.Errorf("i.Language = %v, want %v",got,want)
      }
    }
    log.Printf("cnfProto = %v",cnfProto)
    // Test if we can convert cnf back to TPTP and to proto again
    cnf2,err := ProtoToTptp(ctx,cnfProto)
    if err!=nil { t.Fatalf("ProtoToTPTP(%q): %v",k,err) }
    _,err = eprover.FOFToCNF(ctx,cnf2)
    if err!=nil { t.Fatalf("FOFToCNF(%q): %v",k,err) }
  }
}

func TestTptpHasEquality(t *testing.T) {
  ctx := context.Background()
  problemSet := sample.SampleProblems()
  for _,c := range []struct { name string; want bool } {
    {"trivial", false},
    {"simple", false},
    {"eqAxiom1", true},
    {"eqAxiom2", true},
    {"eqAxiom3", true},
    {"barber", false},
    {"pelletier20", false},
    {"pelletier24", false},
  } {
    tptp,ok := problemSet[c.name]
    if !ok { t.Error("%q not in sample.SampleProblems()",c.name) }
    got,err := TptpHasEquality(ctx,tptp)
    if err!=nil {
      t.Errorf("TptpHasEquality(%q): %v",c.name,err)
    } else if got!=c.want {
      t.Errorf("TptpHasEquality(%q) = %, want %",got,c.want)
    }
  }
}

type NodeIndex struct {
  funs map[string]int32
  preds map[string]int32
  standard_ map[tpb.Type]int32
  nodes []*tpb.Node
}

func newNodeIndex() *NodeIndex {
  return &NodeIndex{
    funs: map[string]int32{},
    preds: map[string]int32{},
    standard_: map[tpb.Type]int32{},
  }
}

func (idx *NodeIndex) fun(name string, arity int) int32 {
  if f,ok := idx.funs[name]; ok { return f }
  id := int32(len(idx.nodes))
  idx.funs[name] = id
  idx.nodes = append(idx.nodes, &tpb.Node{
    Type: tpb.Type_TERM_FUN,
    Id: id,
    Arity: int32(arity),
    Name: name,
  })
  return id
}

func (idx *NodeIndex) pred(name string, arity int) int32 {
  if p,ok := idx.preds[name]; ok { return p }
  id := int32(len(idx.nodes))
  idx.preds[name] = id
  idx.nodes = append(idx.nodes, &tpb.Node{
    Type: tpb.Type_PRED,
    Id: id,
    Arity: int32(arity),
    Name: name,
  })
  return id
}

func (idx *NodeIndex) standard(t tpb.Type) int32 {
  if s,ok := idx.standard_[t]; ok { return s }
  id := int32(len(idx.nodes))
  idx.standard_[t] = id
  idx.nodes = append(idx.nodes, &tpb.Node{
    Type: t,
    Id: id,
  })
  return id
}

func TestValidateProofOK(t *testing.T) {
  idx := newNodeIndex()
  p := idx.pred("p",0)
  neg := idx.standard(tpb.Type_FORM_NEG)
  clauses := []*tpb.Input{
    {
      Language: tpb.Input_CNF,
      Role: tpb.Input_PLAIN,
      Formula: []int32{p},
    },
    {
      Language: tpb.Input_CNF,
      Role: tpb.Input_PLAIN,
      Formula: []int32{neg,p},
    },
  }
  cnfProblem := &tpb.File {
    Input: clauses,
    Nodes: idx.nodes,
  }

  var sources []*spb.Source
  for _,c := range clauses {
    sources = append(sources,&spb.Source{Ground:c,Source:c})
  }
  cnfProof := &spb.Proof {
    Clauses: []*spb.Derivation{{Sources:sources}},
    Nodes: idx.nodes,
  }
  if _,err := ValidateProof(context.Background(),&spb.CNF{Problem:cnfProblem,Proof:cnfProof}); err!=nil {
    t.Errorf("ValidateProof(): %v",err)
  }
}

func TestValidateProofFail(t *testing.T) {
  idx := newNodeIndex()
  p := idx.pred("p",0)
  cnfProblem := &tpb.File {
    Input: []*tpb.Input{
      {
        Language: tpb.Input_CNF,
        Role: tpb.Input_PLAIN,
        Formula: []int32{p},
      },
    },
    Nodes: idx.nodes,
  }
  cnfProof := &spb.Proof {
    Nodes: idx.nodes,
  }
  if stats,err := ValidateProof(context.Background(),&spb.CNF{Problem:cnfProblem,Proof:cnfProof}); err==nil {
    t.Errorf("ValidateProof() = %v, want error",stats)
  }
}
