package tool

import (
  "context"
  "bytes"
  "os"
  "fmt"
  "io/ioutil"
  "os/exec"

  "github.com/pompon0/tptp_benchmark_go/utils"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
  "github.com/golang/protobuf/proto"
)

const hs_tool_bin_path = "__main__/tptp_parser/src/tool"
const cc_tool_bin_path = "__main__/lazyparam_prover/tool"
const tmp_prefix = "tptp_benchmark_go_"

type Language string
const FOF Language = "fof"
const CNF Language = "cnf"

func WriteTmp(data []byte) (path string, cleanup func(), err error) {
  tmpFile,err := ioutil.TempFile("",tmp_prefix)
  if err!=nil { return "",nil,fmt.Errorf("ioutil.TempFile(): %v",err) }
  if _,err := tmpFile.Write(data); err!=nil { return "",nil,fmt.Errorf("tmpFile.Write(): %v",err) }
  return tmpFile.Name(),func(){ os.Remove(tmpFile.Name()) },nil
}

func ProtoToTptp(ctx context.Context, f *tpb.File) ([]byte,error) {
  fBytes,err := proto.Marshal(f)
  if err!=nil { return nil,fmt.Errorf("proto.Marshal(): %v",err) }
  tmp,cleanup,err := WriteTmp(fBytes)
  if err!=nil { return nil,fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()

  var outBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(hs_tool_bin_path),"tptp",tmp)
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err = cmd.Run(); err!=nil { return nil,fmt.Errorf("cmd.Run(): %v",err) }
  return outBuf.Bytes(),nil
}

func ProofToTptp(ctx context.Context, proof *spb.Proof) ([]byte,error) {
  f := &tpb.File{Nodes:proof.Nodes}
  for _,d := range proof.Clauses {
    f.Input = append(f.Input,d.Derived)
    for _,s := range d.Sources {
      f.Input = append(f.Input,s.Ground,s.Source)
    }
  }
  return ProtoToTptp(ctx,f)
}

func TptpToProto(ctx context.Context, lang Language, tptp []byte) (*tpb.File,error) {
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.Write(tptp); err!=nil {
    return nil,fmt.Errorf("inbuf.Write(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(cc_tool_bin_path))
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err := cmd.Run(); err!=nil {
    if ctx.Err()!=nil { return nil,ctx.Err() }
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }
  out := &tpb.ToolOutput{}
  if err:=proto.Unmarshal(outBuf.Bytes(),out); err!=nil {
    return nil,fmt.Errorf("proto.Unmarshal(): %v",err)
  }
  return out.File,nil
}

func TptpHasEquality(ctx context.Context, tptp []byte) (bool,error) {
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.Write(tptp); err!=nil {
    return false,fmt.Errorf("inbuf.Write(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(cc_tool_bin_path))
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err := cmd.Run(); err!=nil {
    if ctx.Err()!=nil { return false,ctx.Err() }
    return false,fmt.Errorf("cmd.Run(): %v",err)
  }
  out := &tpb.ToolOutput{}
  if err:=proto.Unmarshal(outBuf.Bytes(),out); err!=nil {
    return false,fmt.Errorf("proto.Unmarshal(): %v",err)
  }
  return out.HasEquality,nil

}

func ValidateProof(ctx context.Context, sol *spb.CNF) (*spb.Stats,error) {
  solBytes,err := proto.Marshal(sol)
  if err!=nil { return nil,fmt.Errorf("proto.Marshal(): %v",err) }
  tmpSol,cleanup,err := WriteTmp(solBytes)
  if err!=nil { return nil,fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()

  var outBuf,errBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(hs_tool_bin_path),"validate",tmpSol)
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err = cmd.Run(); err!=nil {
    return nil,fmt.Errorf("cmd.Run(): [%v]\n%v",err,errBuf.String())
  }
  stats := &spb.Stats{}
  if err:=proto.Unmarshal(outBuf.Bytes(),stats); err!=nil {
    return nil,fmt.Errorf("proto.Unmarshal(): %v",err)
  }
  return stats,nil
}
