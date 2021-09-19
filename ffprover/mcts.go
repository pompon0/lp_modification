package mcts

import (
  "context"
  "fmt"
  "bytes"
  "time"
  "os"
  "os/exec"
  "syscall"

  "github.com/golang/protobuf/proto"
  "google.golang.org/protobuf/types/known/durationpb"

  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/tool"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const mcts_bin_path = "__main__/ffprover/mcts"

func MCTS(ctx context.Context, input *mpb.Input) (*mpb.Output,error) {
  var inBuf,outBuf bytes.Buffer
  inputBytes,err := proto.Marshal(input)
  if err!=nil { return nil,fmt.Errorf("proto.Marshal(): %w",err) }
  if _,err := inBuf.Write(inputBytes); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %w",err)
  }

  cmd := exec.Command(utils.Runfile(mcts_bin_path))
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  const memLimitBytes = 2*1000*1000*1000
  if err := utils.RunWithMemLimit(ctx,cmd,memLimitBytes); err!=nil {
    status := err.(*exec.ExitError).Sys().(syscall.WaitStatus)
    if status.Signaled() && status.Signal()==syscall.SIGKILL {
      return &mpb.Output{Status: mpb.Status_KILLED},nil
    }
    // deadline exceeded is not acceptable, summary should be always provided.
    return nil,fmt.Errorf("cmd.Run(): %w",err)
  }
  output := &mpb.Output{}
  if err:=proto.Unmarshal(outBuf.Bytes(),output); err!=nil {
    return nil,fmt.Errorf("proto.Unmarshal(): %w",err)
  }
  //TODO: consider populating the output.Problem by the cc binary
  // and here just validating that it doesn't change.
  output.Problem = input.Problem
  return output,nil
}

func Prove(ctx context.Context, fof *tool.TPTP) (*spb.ProverOutput,error) {
  cnfTPTP,err := eprover.FOFToCNF(ctx,fof)
  if err!=nil { return nil,fmt.Errorf("eprover.FOFToCNF(): %w",err) }
  cnf,err := cnfTPTP.ToProto(ctx,tool.CNF)
  if err!=nil { return nil,fmt.Errorf("tool.TptpToProto(): %w",err) }

  timeout := time.Hour
  if deadline,ok := ctx.Deadline(); ok {
    timeout = deadline.Sub(time.Now())
  }
  gracefulExitTimeout := time.Second
  timeout -= gracefulExitTimeout
  input := &mpb.Input{
    Problem: cnf,
    Timeout: durationpb.New(timeout),
    FullSearch: true,
    FeaturesSpaceSize: 1,
  }
  out,err := MCTS(ctx,input)
  if err!=nil {
    return nil,fmt.Errorf("MCTS(): %w",err)
  }
  return &spb.ProverOutput{
    Cost: int64(out.GetStats().GetBigsteps()),
    CnfProblem: cnf,
    Solved: out.GetStatus()==mpb.Status_THEOREM,
    Proof: out.GetProof(),
    Profiler: out.GetProfiler(),
    Killed: out.GetStatus()==mpb.Status_KILLED,
  },nil
}
