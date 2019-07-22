package tool

import (
  "context"
  "bytes"
  "os"
  "fmt"
  "io/ioutil"
  "os/exec"

  "github.com/pompon0/tptp_benchmark_go/utils"
  tpb "github.com/pompon0/tptp_parser/proto/tptp_go_proto"
  "github.com/golang/protobuf/proto"
)

const tool_bin_path = "tptp_parser_bin/checker"
const tmp_prefix = "tptp_benchmark_go_"

type Language string
const FOF Language = "fof"
const CNF Language = "cnf"

func writeTmp(data []byte) (path string, cleanup func(), err error) {
  tmpFile,err := ioutil.TempFile("",tmp_prefix)
  if err!=nil { return "",nil,fmt.Errorf("ioutil.TempFile(): %v",err) }
  if _,err := tmpFile.Write(data); err!=nil { return "",nil,fmt.Errorf("tmpFile.WriteString(): %v",err) }
  return tmpFile.Name(),func(){ os.Remove(tmpFile.Name()) },nil
}

func TptpToProto(ctx context.Context, lang Language, tptp []byte) (*tpb.File,error) {
  tmp,cleanup,err := writeTmp(tptp)
  if err!=nil { return nil,fmt.Errorf("writeTmp(): %v",err) }
  defer cleanup()

  var outBuf,errBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(tool_bin_path),"conv",string(lang),tmp)
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err = cmd.Run(); err!=nil { return nil,fmt.Errorf("cmd.Run(): %v",err) }

  pbFile := &tpb.File{}
  if err:=proto.UnmarshalText(outBuf.String(),pbFile); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return pbFile,nil
}

func FOFToCNF(ctx context.Context, fof *tpb.File) (*tpb.File,error) {
  tmp,cleanup,err := writeTmp([]byte(fof.String()))
  if err!=nil { return nil,fmt.Errorf("writeTmp(): %v",err) }
  defer cleanup()

  var outBuf,errBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(tool_bin_path),"cnf",tmp)
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err = cmd.Run(); err!=nil { return nil,fmt.Errorf("cmd.Run(): %v",err) }

  cnf := &tpb.File{}
  if err:=proto.UnmarshalText(outBuf.String(),cnf); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return cnf,nil
}

func ValidateProof(ctx context.Context, cnfProblem *tpb.File, cnfProof *tpb.File) (bool,error) {
  tmpProblem,cleanup,err := writeTmp([]byte(cnfProblem.String()))
  if err!=nil { return false,fmt.Errorf("writeTmp(): %v",err) }
  defer cleanup()

  tmpProof,cleanup,err := writeTmp([]byte(cnfProof.String()))
  if err!=nil { return false,fmt.Errorf("writeTmp(): %v",err) }
  defer cleanup()

  var outBuf,errBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(tool_bin_path),"validate",tmpProblem,tmpProof)
  cmd.Stdout = &outBuf
  cmd.Stderr = &errBuf
  if err = cmd.Run(); err!=nil {
    return false,fmt.Errorf("cmd.Run(): [%v]\n%v",err,errBuf.String())
  }
  return true,nil
}
