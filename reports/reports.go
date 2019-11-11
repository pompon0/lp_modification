package main

import (
  "context"
  "flag"
  "fmt"
  "log"
  "sort"
  "strings"

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

func summary(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
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
    if err!=nil { log.Printf("ptypes.Duration(): %v",err); continue }
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
