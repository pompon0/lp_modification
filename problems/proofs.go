package problems

import (
  "fmt"
  "path"
  "path/filepath"
  "time"
  "io/ioutil"
  "os"

  "github.com/golang/protobuf/proto"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

func ReadProofs(proofDir string, name string) ([]*spb.CNF,error) {
  p := path.Join(proofDir,name)
  var solutions []*spb.CNF
  if _,err := os.Stat(p); os.IsNotExist(err) { return nil,nil }
  if err := filepath.Walk(p,func(fp string, fi os.FileInfo, err error) error {
    if err!=nil { return err }
    if fi.IsDir() { return nil }
    raw,err := ioutil.ReadFile(fp)
    if err!=nil { return fmt.Errorf("ioutil.ReadAll(): %v",err) }
    solution := &spb.CNF{}
    if err:=proto.UnmarshalText(string(raw),solution); err!=nil {
      return fmt.Errorf("proto.UnmarshalText(): %v",err)
    }
    solutions = append(solutions,solution)
    return nil
  }); err!=nil { return nil,fmt.Errorf("filepath.Walk(): %v",err) }
  return solutions,nil
}

func WriteProof(proofDir string, name string, solution *spb.CNF) (bool,error) {
  p := path.Join(proofDir,name)
  if _,err := os.Stat(p); os.IsNotExist(err) {
    if err := os.MkdirAll(p,0777); err!=nil {
      return false,fmt.Errorf("os.MkDirAll(): %v",err)
    }
  }
  oldSolutions,err := ReadProofs(proofDir,name)
  if err!=nil{ return false,fmt.Errorf("readProofs(%q): %v",name,err) }
  found := false
  for _,s := range oldSolutions {
    found = found || proto.Equal(solution,s)
  }
  if !found {
    err := ioutil.WriteFile(path.Join(p,time.Now().Format("2006-01-02_15:04:05")),[]byte(solution.String()),0666)
    if err!=nil { return false,fmt.Errorf("ioutil.WriteFile(): %v",err) }
  }
  return len(oldSolutions)==0,nil
}

func WriteReport(reportDir string, report *spb.Report) error {
  reportString := (&proto.TextMarshaler{Compact:false}).Text(report)
  return ioutil.WriteFile(path.Join(reportDir,time.Now().Format("2006-01-02_15:04:05")),[]byte(reportString),0666)
}

func ReadReport(reportPath string) (*spb.Report, error) {
  raw,err := ioutil.ReadFile(reportPath)
  if err!=nil { return nil,fmt.Errorf("ioutil.ReadFile(): %v",err) }
  report := &spb.Report{}
  if err:=proto.UnmarshalText(string(raw),report); err!=nil {
    return nil,fmt.Errorf("proto.UnmarshalText(): %v",err)
  }
  return report,nil
}
