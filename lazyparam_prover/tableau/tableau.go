package tableau

import (
  "fmt"
  "bytes"
  "os/exec"
  "os"
  "context"
  "time"

  "github.com/pompon0/tptp_benchmark_go/utils"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/golang/protobuf/proto"
)

const tableau_bin_path = "__main__/lazyparam_prover/main"

func Prove(ctx context.Context, tptpFOFProblem []byte) error {
  fof,err := tool.TptpToProto(ctx,tool.FOF,tptpFOFProblem)
  if err!=nil { return fmt.Errorf("tool.TptpToProto(): %v",err) }
  cnf,err := tool.FOFToCNF(ctx,fof)
  if err!=nil { return fmt.Errorf("tool.FOFToCNF(): %v",err) }
  out,err := Tableau(ctx,cnf,false,false)
  if err!=nil { return err }
  if out.Proof==nil { panic("proof not found") }
  return err
}

func Tableau(ctx context.Context, cnfProblem *tpb.File, streamStdErr bool, graceful bool) (*spb.ProverOutput,error) {
  var inBuf,outBuf,errBuf bytes.Buffer
  cnfProblemBytes, err := proto.Marshal(cnfProblem)
  if err!=nil { return nil,fmt.Errorf("proto.Marshal(): %v",err) }
  if _,err := inBuf.Write(cnfProblemBytes); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }
  timeout := time.Hour
  if deadline,ok := ctx.Deadline(); ok {
    timeout = deadline.Sub(time.Now())
  }
  if graceful {
    gracefulExitTimeout := 100*time.Millisecond
    timeout -= gracefulExitTimeout
  } else {
    timeout += time.Hour
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(tableau_bin_path),fmt.Sprintf("--timeout=%v",timeout))
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  if streamStdErr {
    cmd.Stderr = os.Stderr
  } else {
    cmd.Stderr = &errBuf
  }
  if err := cmd.Run(); err!=nil {
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }

  output := &spb.ProverOutput{}
  if err:=proto.Unmarshal(outBuf.Bytes(),output); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return output,nil
}
