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

  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

const (
  cmdSummary = "summary"
  cmdPrint = "print"
  cmdDiff = "diff"
  cmdMultiDiff = "multidiff"
  cmdList = "list"
  cmdProfiler = "profiler"
  cmdProfilerDiff = "profilerdiff"
)
var cmds = []string{cmdSummary,cmdPrint,cmdDiff,cmdMultiDiff,cmdList,cmdProfiler,cmdProfilerDiff}

var reportDir = flag.String("report_dir","","")
var reportPath = flag.String("report_path","","")
var reportPath2 = flag.String("report_path_2","","")
var cmd = flag.String("cmd",cmdSummary,strings.Join(cmds,"|"))
var caseName = flag.String("case_name","","")

var profilerLabel = flag.String("profiler_label","","label used as a reference unit of the number of cycles")

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
  d := c.Duration.AsDuration()
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
    d := c.Duration.AsDuration()
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

  reports := []*spb.Report{report1,report2}
  var results []map[string]*Result
  for i,r := range reports {
    fmt.Printf("[%d] commit = %q, labes = %v\n",i,r.Commit,r.Labels)
    res,err := accumByPrefix(r)
    if err!=nil { return err }
    results = append(results,res)
  }
  calcUnique(reports,results)
  lines := map[string]string{}
  total := map[string]int{}
  for _,res := range results {
    for k,v := range res {
      lines[k] += fmt.Sprintf(" & %d & %d",v.unique,v.solved)
      total[k] = v.total
    }
  }
  var keys []string
  for k,_ := range lines {
    if strings.Count(k,"/")>0 { continue }
    keys = append(keys,k)
  }
  sort.Strings(keys)
  for _,k := range keys {
    fmt.Printf("%s%s & %d\\\\\n",k,lines[k],total[k])
  }
  return nil
}

type Result struct {
  solved int
  total int
  unique int
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
      res[p].totalTime += c.Duration.AsDuration()
    }
  }
  return res,nil
}

func calcUnique(r []*spb.Report, m []map[string]*Result) {
  for ri:=0; ri<len(r); ri++ {
    for _,c := range r[ri].Cases {
      if m[ri][c.Name].solved==0 { continue }
      unique := true
      for rj:=0; rj<len(m); rj++ {
        if m[rj][c.Name].solved>0 && ri!=rj { unique = false }
      }
      if unique {
        labels := strings.Split(c.Name,"/")
        for i:=0; i<=len(labels); i++ {
          m[ri][strings.Join(labels[:i],"/")].unique++
        }
      }
    }
  }
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
  total := map[string]int{}
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
        lines[k] += fmt.Sprintf(" & %d",v.solved)
        total[k] = v.total
      }
    }
  }
  var keys []string
  for k,_ := range lines {
    keys = append(keys,k)
  }
  sort.Strings(keys)
  for _,k := range keys {
    fmt.Printf("%s %s & %d\\\\\n",k,lines[k],total[k])
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
      fmt.Printf("%s\n\n",string(tptp.Raw))
      if c.Output!=nil {
        tptpProof,err := tool.ProofToTptp(ctx,c.Output.Proof)
        if err!=nil { return fmt.Errorf("ProtoToTptp(%q[proof]): %v",c.Name,err) }
        fmt.Printf("%s\n\n",string(tptpProof.Raw))
      }
    }
  }
  return nil
}

func list(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  var solved []string
  for _,c := range report.Cases {
    if c.GetOutput().GetSolved() {
      solved = append(solved,fmt.Sprintf("%q: true,\n",c.Name))
    }
  }
  sort.Strings(solved)
  fmt.Printf("%s",strings.Join(solved,""))
  return nil
}

type ProfileEntry struct {
  Count uint64
  Cycles uint64
  TimeSec float64
}

func NewProfileEntry(e *spb.Profiler_Entry) ProfileEntry {
  return ProfileEntry{e.Count,e.Cycles,e.TimeS}
}

func (e ProfileEntry) Less(b ProfileEntry) bool {
  if x,y := e.Cycles,b.Cycles; x!=y { return x>y }
  if x,y := e.Count,b.Count; x!=y { return x>y }
  return false
}

func entryLess(a,b *spb.Profiler_Entry) bool {
  if x,y := a.Cycles,b.Cycles; x!=y { return x<y }
  if x,y := a.Count,b.Count; x!=y { return x<y }
  return false
}

func (e ProfileEntry) Add(b ProfileEntry) ProfileEntry {
  e.Count += b.Count
  e.Cycles += b.Cycles
  e.TimeSec += b.TimeSec
  return e
}

type Profile struct {
  Total int
  Entries map[string]ProfileEntry
}

func NewProfile() *Profile {
  return &Profile{Total:0,Entries:map[string]ProfileEntry{}}
}

func (p *Profile) Acc(pp *spb.Profiler) {
  p.Total++
  for _,e := range pp.Entries {
    p.Entries[e.Label] = p.Entries[e.Label].Add(NewProfileEntry(e))
  }
}

type ProfileByPrefix map[string]*Profile

func (bp ProfileByPrefix) Acc(c *spb.Case) {
  labels := strings.Split(c.Name,"/")
  for i:=0; i<len(labels); i++ {
    p := strings.Join(labels[:i],"/")
    if bp[p]==nil { bp[p] = NewProfile() }
    bp[p].Acc(c.GetOutput().GetProfiler())
  }
}

func profiler(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }

  pbp := ProfileByPrefix{}
  for _,c := range report.Cases {
    if c.GetOutput().GetSolved() { pbp.Acc(c) }
  }
  for n,p := range pbp {
    fmt.Printf("\n[%s] %v\n",n,p.Total)
    type LabelEntry struct{ label string; e ProfileEntry }
    var es []LabelEntry
    for l,e := range p.Entries {
      es = append(es,LabelEntry{l,e})
    }
    sort.Slice(es,func(i,j int) bool { return es[i].e.Less(es[j].e) })
    if unit := p.Entries[*profilerLabel].Cycles; unit>0 {
      for _,e := range es {
        fmt.Printf("\t%.5f :: %s\n",float64(e.e.Cycles)/float64(unit),e.label)
      }
    } else {
      for _,e := range es {
        fmt.Printf("\t%s :: %+v\n",e.label,e.e)
      }
    }
  }
  return nil
}

func profilerDiff(ctx context.Context) error {
  report1,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  report2,err := problems.ReadReport(*reportPath2)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath2,err) }

  cs1 := map[string]*spb.Case{}
  for _,c := range report1.Cases {
    cs1[c.Name] = c
  }
  cs2 := map[string]*spb.Case{}
  for _,c := range report2.Cases {
    cs2[c.Name] = c
  }

  fmt.Printf("%q | %q\n",*reportPath,*reportPath2)
  for n,c1 := range cs1 {
    c2,ok := cs2[n]
    if !ok { continue }
    if c1.GetOutput().GetSolved() || !c2.GetOutput().GetSolved() { continue }
    fmt.Printf("\n[%s] %v %v | %v %v\n",n,
      c1.GetDuration().AsDuration(), c1.GetOutput().GetSolved(),
      c2.GetDuration().AsDuration(), c2.GetOutput().GetSolved(),
    )
    fmt.Printf("\tcost = %v | cost = %v\n",c1.GetOutput().GetCost(),c2.GetOutput().GetCost())
    es := c1.GetOutput().GetProfiler().GetEntries()
    sort.Slice(es,func(i,j int) bool { return entryLess(es[j],es[i]) })
    es2 := map[string]*spb.Profiler_Entry{}
    for _,e := range c2.GetOutput().GetProfiler().GetEntries() {
      es2[e.GetLabel()] = e
    }
    for _,e := range es {
      fmt.Printf("\t%+v | %+v\n",e,es2[e.GetLabel()])
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
    case cmdList: return list(ctx)
    case cmdProfiler: return profiler(ctx)
    case cmdProfilerDiff: return profilerDiff(ctx)
    default: return fmt.Errorf("unknown command = %q",*cmd)
  }
}

func main() {
  flag.Parse()
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
