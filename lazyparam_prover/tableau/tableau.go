package tableau

import (
  "fmt"
  "bytes"
  "os/exec"
  "os"
  "context"
  "time"
  "syscall"
  //"log"

  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/golang/protobuf/proto"
  tpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
  ppb "github.com/pompon0/tptp_benchmark_go/lazyparam_prover/prover_go_proto"
)

const tableau_bin_path = "__main__/lazyparam_prover/main"

func ProveAxiomaticEq(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  return Prove(ctx,tptpFOFProblem,nil,ppb.Method_CONNECTION_TABLEAU,ppb.Transformation_AXIOMATIC_EQ,false)
}

func ProveLPModification(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  return Prove(ctx,tptpFOFProblem,nil,ppb.Method_CONNECTION_TABLEAU,ppb.Transformation_LP_MODIFICATION,false)
}

func ProveLazyParamodulation(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  return Prove(ctx,tptpFOFProblem,nil,ppb.Method_LAZY_PARAMODULATION,ppb.Transformation_SKIP,false);
}

func ProveNoEq(ctx context.Context, tptpFOFProblem []byte) (*spb.ProverOutput,error) {
  return Prove(ctx,tptpFOFProblem,nil,ppb.Method_CONNECTION_TABLEAU,ppb.Transformation_SKIP,false);
}

func Prove(ctx context.Context, tptpFOFProblem []byte, funOrd *spb.FunOrd, method ppb.Method, trans ppb.Transformation, transOnly bool) (*spb.ProverOutput,error) {
  tptpCNF,err := eprover.FOFToCNF(ctx,tptpFOFProblem)
  if err!=nil { return nil,fmt.Errorf("eprover.FOFToCNF(): %v",err) }
  //log.Printf("tptpCNF = %v",string(tptpCNF))
  cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
  if err!=nil { return nil,fmt.Errorf("tool.TptpToProto(): %v",err) }
  //log.Printf("cnf = %v",cnf)
  out,err := Tableau(ctx,cnf,funOrd,true,method,trans,transOnly)
  if err!=nil {
    if err==context.DeadlineExceeded {
      return &spb.ProverOutput{Solved:false},nil
    }
    return nil,fmt.Errorf("Tableau(): %v",err)
  }
  out.CnfProblem = cnf
  return out,nil
}

func Tableau(ctx context.Context, cnfProblem *tpb.File, funOrd *spb.FunOrd, streamStdErr bool, method ppb.Method, trans ppb.Transformation, transOnly bool) (*spb.ProverOutput,error) {
  var inBuf,outBuf,errBuf bytes.Buffer
  cnfProblemBytes, err := proto.Marshal(&spb.ProverInput{Problem:cnfProblem,FunOrd:funOrd})
  if err!=nil { return nil,fmt.Errorf("proto.Marshal(): %v",err) }
  if _,err := inBuf.Write(cnfProblemBytes); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }
  timeout := time.Hour
  if deadline,ok := ctx.Deadline(); ok {
    timeout = deadline.Sub(time.Now())
  }
  gracefulExitTimeout := 200*time.Millisecond
  timeout -= gracefulExitTimeout
  cmd := exec.Command(utils.Runfile(tableau_bin_path),
    fmt.Sprintf("--timeout=%v",timeout),
    fmt.Sprintf("--trans=%v",trans),
    fmt.Sprintf("--method=%v",method),
    fmt.Sprintf("--trans_only=%v",transOnly),
  )
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  if streamStdErr {
    cmd.Stderr = os.Stderr
  } else {
    cmd.Stderr = &errBuf
  }
  const memLimitBytes = 2*1000*1000*1000
  if err := utils.RunWithMemLimit(ctx,cmd,memLimitBytes); err!=nil {
    status := err.(*exec.ExitError).Sys().(syscall.WaitStatus)
    if status.Signaled() && status.Signal()==syscall.SIGKILL {
      return &spb.ProverOutput{Solved:false,Killed:true},nil
    }
    // deadline exceeded is not acceptable, summary should be always provided.
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }

  output := &spb.ProverOutput{}
  if err:=proto.Unmarshal(outBuf.Bytes(),output); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return output,nil
}
