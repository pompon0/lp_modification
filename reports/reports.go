package main

import (
  "context"
  "flag"
  "fmt"
  "log"
  "sort"
  "strings"
  "time"

  "github.com/golang/protobuf/ptypes"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

const (
  cmdSummary = "summary"
  cmdPrint = "print"
)
var cmds = []string{cmdSummary,cmdPrint}

var reportPath = flag.String("report_path","","")
var cmd = flag.String("cmd",cmdSummary,strings.Join(cmds,"|"))
var caseName = flag.String("case_name","","")


type Rec struct {
  caseCount int
  totalCont int64
  totalTime time.Duration
}

type Stats map[int64]Rec

func NewStats() Stats { return map[int64]Rec{} }

func (s Stats) Add(cost int64, r Rec) {
  s[cost] = Rec{s[cost].caseCount+r.caseCount,s[cost].totalCont+r.totalCont,s[cost].totalTime+r.totalTime}
}

func (s Stats) String() string {
  var keys []int64
  for k,_ := range s { keys = append(keys,k) }
  sort.Slice(keys,func(i,j int) bool { return keys[i]<keys[j]})

  var out []string
  for _,cost := range keys {
    r := s[cost]
    if r.caseCount==0 { continue }
    out = append(out,fmt.Sprintf(
      "%03d (%d cases) -> cont/case = %.2f,  cont/sec = %.2f\n",
      cost,r.caseCount,float64(r.totalCont)/float64(r.caseCount),float64(r.totalCont)/r.totalTime.Seconds()))
  }
  return strings.Join(out,"")
}

func summary(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  stats := NewStats()
  for _,c := range report.Cases {
    if c.GetOutput().GetProof()==nil { continue }
    d,err := ptypes.Duration(c.Duration)
    if err!=nil { log.Printf("ptypes.Duration(%q): %v",c.Name,err); continue }
    stats.Add(c.Output.Cost,Rec{1,c.Output.ContinuationCount,d})
  }
  fmt.Printf("%v",stats)

  sort.Slice(report.Cases, func(i,j int) bool {
    ad,err := ptypes.Duration(report.Cases[i].Duration)
    if err!=nil { panic("ptypes.Duration()"); }
    bd,err := ptypes.Duration(report.Cases[j].Duration)
    if err!=nil { panic("ptypes.Duration()"); }
    return ad < bd
  })
  for _,c := range report.Cases {
    if c.GetOutput().GetProof()==nil { continue }
    d,err := ptypes.Duration(c.Duration)
    if err!=nil { log.Printf("ptypes.Duration(%q): %v",c.Name,err); continue }
    fmt.Printf("%20q : %05.2f cost=%3d cont=%d\n",c.Name,d.Seconds(),c.Output.Cost,c.Output.ContinuationCount)
  }
  return nil
}

func print_(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  for _,c := range report.Cases {
    if c.Name == *caseName {
      tptp,err := tool.ProtoToTptp(ctx,c.CnfProblem)
      if err!=nil { return fmt.Errorf("ProtoToTptp(%q): %v",c.Name,err) }
      fmt.Printf("%s\n\n",string(tptp))
      if c.Output!=nil {
        tptpProof,err := tool.ProtoToTptp(ctx,c.Output.Proof)
        if err!=nil { return fmt.Errorf("ProtoToTptp(%q[proof]): %v",c.Name,err) }
        fmt.Printf("%s\n\n",string(tptpProof))
      }
    }
  }
  return nil
}

func run(ctx context.Context) error {
  switch *cmd {
    case cmdSummary: return summary(ctx)
    case cmdPrint: return print_(ctx)
    default: return fmt.Errorf("unknown command = %q",*cmd)
  }
}

func main() {
  flag.Parse()
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
