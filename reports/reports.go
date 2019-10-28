package main

import (
  "context"
  "flag"
  "fmt"
  "log"
  "sort"

  "github.com/golang/protobuf/ptypes"
  "github.com/pompon0/tptp_benchmark_go/problems"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

var reportPath = flag.String("report_path","","")

type caseSorter struct {
  arr []*spb.Case
  cmp func(a,b *spb.Case) bool
}

func (s caseSorter) Len() int { return len(s.arr) }
func (s caseSorter) Less(i,j int) bool { return s.cmp(s.arr[i],s.arr[j]) }
func (s caseSorter) Swap(i,j int) { s.arr[i],s.arr[j] = s.arr[j],s.arr[i] }

func run(ctx context.Context) error {
  report,err := problems.ReadReport(*reportPath)
  if err!=nil { return fmt.Errorf("problems.ReadReport(report_path=%q): %v",*reportPath,err) }
  sort.Sort(caseSorter{report.Cases,func(a,b *spb.Case) bool {
    ad,err := ptypes.Duration(a.Duration)
    if err!=nil { panic("ptypes.Duration()"); }
    bd,err := ptypes.Duration(b.Duration)
    if err!=nil { panic("ptypes.Duration()"); }
    return ad < bd
  }})
  for _,c := range report.Cases {
    if c.GetOutput().GetProof()==nil { continue }
    d,err := ptypes.Duration(c.Duration)
    if err!=nil { log.Printf("ptypes.Duration(): %v",err); continue }
    fmt.Printf("%20q : %05.2f cost=%3d cont=%d\n",c.Name,d.Seconds(),c.Output.Cost,c.Output.ContinuationCount)
  }
  return nil
}

func main() {
  flag.Parse()
  if err := run(context.Background()); err!=nil {
    log.Fatalf("%v",err)
  }
}
