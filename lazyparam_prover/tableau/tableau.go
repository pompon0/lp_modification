package tableau

import (
  "fmt"
  "bytes"
  "os/exec"
  "os"
  "context"
  "time"

  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/golang/protobuf/proto"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const tableau_bin_path = "__main__/lazyparam_prover/main"

func Prove(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  tptpCNF,err := eprover.FOFToCNF(ctx,tptpFOFProblem)
  if err!=nil { return nil,fmt.Errorf("eprover.FOFToCNF(): %v",err) }
  cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
  if err!=nil { return nil,fmt.Errorf("tool.TptpToProto(): %v",err) }
  out,err := Tableau(ctx,cnf,true,true)
  if err!=nil {
    if err==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    return nil,fmt.Errorf("Tableau(): %v",err)
  }
  out.CnfProblem = cnf
  return out,nil
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
    if ctx.Err()==context.DeadlineExceeded { return nil,ctx.Err() }
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }

  output := &spb.ProverOutput{}
  if err:=proto.Unmarshal(outBuf.Bytes(),output); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return output,nil
}
