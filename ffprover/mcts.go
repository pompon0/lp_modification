package mcts

import (
  "context"
  "fmt"
  "bytes"
  "os"
  "os/exec"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/utils"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
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
    return nil,fmt.Errorf("cmd.Run(): %w",err)
  }
  output := &mpb.Output{}
  if err:=proto.Unmarshal(outBuf.Bytes(),output); err!=nil {
    return nil,fmt.Errorf("proto.Unmarshal(): %w",err)
  }
  return output,nil
}
