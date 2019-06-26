package tableau

import (
  "fmt"
  "bytes"
  "os/exec"
  "os"
  "context"

  "github.com/pompon0/tptp_benchmark_go/utils"
  tpb "github.com/pompon0/tptp_parser/proto/tptp_go_proto"
  "github.com/golang/protobuf/proto"
)

const tableau_bin_path = "lazyparam_prover/main"

func Tableau(ctx context.Context, cnfProblem *tpb.File) (*tpb.File,error) {
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.WriteString(cnfProblem.String()); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(tableau_bin_path))
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  //cmd.Stderr = &errBuf
  if err := cmd.Run(); err!=nil {
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }

  cnfProof := &tpb.File{}
  if err:=proto.UnmarshalText(outBuf.String(),cnfProof); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return cnfProof,nil
}
