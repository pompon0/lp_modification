package leancop

import (
  "fmt"
  "bytes"
  "context"
  "os/exec"
  "strings"
  "time"

  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const swiplBinPath = "/usr/bin/swipl"
const leancopMainPath = "leancop_prolog/leancop21/leancop_main.pl"

func makeCmd(ctx context.Context, strategy string, inputPath string) *exec.Cmd {
  program := `
    assert((print(A):-write(A))),
    assert(prolog(swi)),
    assert(proof(compact)),
    ['%s'],
    leancop_main('%s',%s,_).`
  swiplArgs := []string {
    "-t",
    "halt",
    "--no-debug",
    "--stack_limit=1G",
    "--quiet",
    "-g",
    fmt.Sprintf(program,utils.Runfile(leancopMainPath),inputPath,strategy),
  }
  return exec.CommandContext(ctx,swiplBinPath,swiplArgs...)
}

func PrologProve(ctx context.Context, fofProblem *tool.TPTP) (*spb.ProverOutput,error) {
  return prologProveWithSchedule(ctx,fofProblem,schedule)
}

func PrologProveCutComp7(ctx context.Context, fofProblem *tool.TPTP) (*spb.ProverOutput,error) {
  return prologProveWithSchedule(ctx,fofProblem,cutComp7)
}

func prologProveWithSchedule(ctx context.Context, fofProblem *tool.TPTP, schedule []Strategy) (*spb.ProverOutput,error) {
  tmp,cleanup,err := tool.WriteTmp(fofProblem.Raw)
  if err!=nil { return nil,fmt.Errorf("WriteTmp(): %v",err) }
  defer cleanup()
  totalSchedule := 0.
  for _,s := range schedule {
    totalSchedule += float64(s.timeout)
  }
  timeout := totalSchedule
  if deadline,ok := ctx.Deadline(); ok {
    timeout = float64(deadline.Sub(time.Now()))
  }
  SCHEDULE: for i,s := range schedule {
    var inBuf,outBuf,errBuf bytes.Buffer
    sctx := ctx
    if i!=len(schedule)-1 {
      var cancel func()
      sctx,cancel = context.WithTimeout(sctx,
        time.Duration(float64(s.timeout)*timeout/totalSchedule))
      defer cancel()
    }
    cmd := makeCmd(sctx,s.strategy,tmp)
    cmd.Stdin = &inBuf
    cmd.Stdout = &outBuf
    cmd.Stderr = &errBuf
    if err := cmd.Run(); err!=nil {
      if ctx.Err()==context.DeadlineExceeded { break }
      if sctx.Err()==context.DeadlineExceeded { continue }
      return nil,fmt.Errorf("cmd.Run(): %q %v",errBuf.String(),err)
    }
    lines := strings.Split(strings.TrimSpace(outBuf.String()),"\n")
    wantTheorem := fmt.Sprintf("%s is a Theorem",tmp)
    wantNonTheorem := fmt.Sprintf("%s is a Non-Theorem",tmp)
    wantSatisfiable := fmt.Sprintf("%s is Satisfiable",tmp)
    wantUnsatisfiable := fmt.Sprintf("%s is Unsatisfiable",tmp)
    for _,l := range lines {
      switch l {
        case wantTheorem:
          return &spb.ProverOutput{Solved:true},nil
        case wantUnsatisfiable:
          return &spb.ProverOutput{Solved:true},nil
        case wantNonTheorem:
          continue SCHEDULE
        case wantSatisfiable:
          continue SCHEDULE
      }
    }
    return nil,fmt.Errorf("status line not found: %q",outBuf.String())
  }
  return &spb.ProverOutput{Solved:false},nil
}

type Strategy struct {
  strategy string
  timeout time.Duration
}

var cutComp7 = []Strategy{
  {"[cut,comp(7)]",10*time.Second},
}

var schedule = []Strategy{
  {"[cut,comp(7)]",10*time.Second},
  {"[conj,def,cut]",15*time.Second},
  {"[nodef,scut,cut]",15*time.Second},
  {"[scut]",10*time.Second},
  {"[def,cut]",5*time.Second},
  {"[conj,nodef,cut]",4*time.Second},
  {"[def,scut,cut]",2*time.Second},
  {"[scut,cut]",2*time.Second},
  {"[conj,def]",1*time.Second},
  {"[reo(40),conj,nodef,scut,cut]",4*time.Second},
  {"[reo(42),def,scut,cut]",4*time.Second},
  {"[reo(12),def,scut,cut]",2*time.Second},
  {"[reo(72),def,scut,cut]",2*time.Second},
  {"[reo(39),nodef,cut]",2*time.Second},
  {"[reo(38),conj,def,cut]",2*time.Second},
  {"[reo(15),conj,def,cut]",2*time.Second},
  {"[reo(73),conj,def,cut]",1*time.Second},
  {"[reo(57),conj,def,cut]",1*time.Second},
  {"[reo(13),conj,nodef,scut,cut]",1*time.Second},
  {"[reo(59),conj,nodef,scut,cut]",1*time.Second},
  {"[reo(75),conj,nodef,scut,cut]",1*time.Second},
  {"[reo(36),conj,nodef,scut]",1*time.Second},
  {"[reo(16),conj,nodef,scut]",1*time.Second},
  {"[reo(71),def,scut]",1*time.Second},
  {"[reo(58),def,scut,cut]",1*time.Second},
  {"[reo(76),def,scut,cut]",1*time.Second},
  {"[reo(74),nodef,cut]",1*time.Second},
  {"[reo(14),nodef,cut]",1*time.Second},
  {"[reo(37),nodef,scut]",1*time.Second},
  {"[def]",0},
}
