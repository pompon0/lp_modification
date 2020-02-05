package main

import (
  "context"
  "flag"
  "fmt"
  "log"
  "sort"
  "strings"
  "time"
  "os"
  "path/filepath"

  "github.com/golang/protobuf/ptypes"
  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const (
  cmdSummary = "summary"
  cmdPrint = "print"
  cmdDiff = "diff"
  cmdMultiDiff = "multidiff"
)
var cmds = []string{cmdSummary,cmdPrint,cmdDiff,cmdMultiDiff}

var reportDir = flag.String("report_dir","","")
var reportPath = flag.String("report_path","","")
var reportPath2 = flag.String("report_path_2","","")
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

func caseSummary(c *spb.Case) string {
  if c.GetOutput()==nil {
    return fmt.Sprintf("%20q : ERROR",c.Name)
  }
  status := ""
  if !c.GetOutput().GetSolved() {
    status = "UNSOLVED"
  }
  d,err := ptypes.Duration(c.Duration)
  if err!=nil { return fmt.Sprintf("ptypes.Duration(%q): %v",c.Name,err); }
  return fmt.Sprintf("%20q : %s %05.2f cost=%3d cont=%d",c.Name,status,d.Seconds(),c.Output.Cost,c.Output.ContinuationCount)
}

func summary(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  stats := NewStats()
  fmt.Printf("labels = %v\n",report.Labels)
  fmt.Printf("commit = %s\n",report.Commit)
  solved := 0
  for _,c := range report.Cases {
    if !c.GetOutput().GetSolved() { continue }
    solved += 1
    d,err := ptypes.Duration(c.Duration)
    if err!=nil { log.Printf("ptypes.Duration(%q): %v",c.Name,err); continue }
    stats.Add(c.Output.Cost,Rec{1,c.Output.ContinuationCount,d})
  }
  fmt.Printf("solved %d/%d\n",solved,len(report.Cases))
  fmt.Printf("%v",stats)
  fmt.Printf("-----------------------------\n")

  sort.Slice(report.Cases, func(i,j int) bool { return report.Cases[i].Name < report.Cases[j].Name })
  for _,c := range report.Cases { fmt.Printf("%s\n",caseSummary(c)) }
  return nil
}

func diff(ctx context.Context) error {
  report1,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  report2,err := problems.ReadReport(*reportPath2)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath2,err) }

  fmt.Printf("[L] commit = %q, labes = %v\n",report1.Commit,report1.Labels)
  fmt.Printf("[R] commit = %q, labes = %v\n",report2.Commit,report2.Labels)

  sort.Slice(report1.Cases, func(i,j int) bool { return report1.Cases[i].Name < report1.Cases[j].Name })
  sort.Slice(report2.Cases, func(i,j int) bool { return report2.Cases[i].Name < report2.Cases[j].Name })
  cc1 := len(report1.Cases)
  cc2 := len(report2.Cases)
  if cc1!=cc2 {
    return fmt.Errorf("number of cases mismatch len(report1) = %d, len(report2) = %d",cc1,cc2)
  }

  var lines []string
  unique1 := map[string]int{}
  unique2 := map[string]int{}
  for i:=0; i<cc1; i++ {
    c1 := report1.Cases[i]
    c2 := report2.Cases[i]
    if c1.Name!=c2.Name {
      return fmt.Errorf("case name mismatch: report1.Cases[%d] = %q, report2.Cases[%d] = %q",i,c1.Name,i,c2.Name)
    }
    c1s := c1.GetOutput().GetSolved()
    c2s := c2.GetOutput().GetSolved()
    if c1s!=c2s {
      labels := strings.Split(c1.Name,"/")
      for i:=0; i<len(labels); i++ {
        p := strings.Join(labels[:i],"/")
        if c1s { unique1[p]++ }
        if c2s { unique2[p]++ }
      }
      lines = append(lines,fmt.Sprintf("%s  |  %s\n",caseSummary(c1),caseSummary(c2)))
    }
  }
  var keys []string
  for k,_ := range unique1 { keys = append(keys,k) }
  for k,_ := range unique2 { keys = append(keys,k) }
  sort.Strings(keys)
  prev := ""
  for _,k := range keys {
    if prev==k { continue }
    prev = k
    fmt.Printf("%10s | %10d | %10d\n",k,unique1[k],unique2[k])
  }
  fmt.Printf("%s",strings.Join(lines,""))
  return nil
}

type Result struct {
  solved int
  total int
  totalTime time.Duration
}

func (r *Result) String() string { return fmt.Sprintf("%3d/%3d",r.solved,r.total) }

func accumByPrefix(r *spb.Report) (map[string]*Result,error) {
  res := map[string]*Result{}
  for _,c := range r.Cases {
    labels := strings.Split(c.Name,"/")
    for i:=0; i<=len(labels); i++ {
      p := strings.Join(labels[:i],"/")
      if res[p]==nil { res[p] = &Result{} }
      if c.GetOutput().GetSolved() { res[p].solved++ }
      res[p].total++
      d,err := ptypes.Duration(c.Duration)
      if err!=nil { return nil,err }
      res[p].totalTime += d
    }
  }
  return res,nil
}

func multidiff(ctx context.Context) error {
  var reports []*spb.Report
  if err := filepath.Walk(*reportDir,func(p string, fi os.FileInfo, err error) error {
    if err!=nil { return err }
    if fi.IsDir() { return nil }
    r,err := problems.ReadReport(p)
    if err!=nil { return fmt.Errorf("problems.ReadReport(%q): %v",p,err) }
    reports = append(reports,r)
    return nil
  }); err!=nil { return fmt.Errorf("filepath.Walk(reportDir=%q): %v",*reportDir,err) }
  if len(reports)==0 {
    return fmt.Errorf("no reports found under %q",*reportDir)
  }
  lines := map[string]string{}
  for i,r := range reports {
    fmt.Printf("[%d] commit = %q, labes = %v\n",i,r.Commit,r.Labels)
    res,err := accumByPrefix(r)
    if err!=nil { return err }
    for k,v := range res {
      if v.total==1 { continue
        var t string
        if v.solved==1 {
          t = fmt.Sprintf("%05.2f",v.totalTime.Seconds())
        } else {
          t = "-----"
        }
        lines[k] += fmt.Sprintf(" |     %s",t)
      } else {
        lines[k] += fmt.Sprintf(" | %4d/%4d",v.solved,v.total)
      }
    }
  }
  var keys []string
  for k,_ := range lines { keys = append(keys,k) }
  sort.Strings(keys)
  for _,k := range keys {
    fmt.Printf("%25s%s\n",k,lines[k])
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
        tptpProof,err := tool.ProofToTptp(ctx,c.Output.Proof)
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
    case cmdDiff: return diff(ctx)
    case cmdMultiDiff: return multidiff(ctx)
    default: return fmt.Errorf("unknown command = %q",*cmd)
  }
}

func main() {
  flag.Parse()
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
