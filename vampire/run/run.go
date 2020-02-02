package main

import (
  "context"
  "log"
  "fmt"
  "flag"

  "github.com/pompon0/tptp_benchmark_go/vampire"
  "github.com/pompon0/tptp_benchmark_go/problems"
)

var caseName = flag.String("case_name","","")

func run(ctx context.Context) error {
  mp,cancel,err := problems.MizarProblems()
  if err!=nil { return fmt.Errorf("problems.MizarProblems(): %v",err) }
  defer cancel()
  tp,cancel,err := problems.TptpProblems()
  if err!=nil { return fmt.Errorf("problems.TptpProblems(): %v",err) }
  defer cancel()
  p,ok := mp[*caseName]
  if !ok {
    p,ok = tp[*caseName]
  }
  if !ok { return fmt.Errorf("case %q not found",*caseName) }
  tptp,err := p.Get()
  if err!=nil { return fmt.Errorf("p.Get(): %v",err) }
  log.Printf("%q\n",tptp)
  out,err := vampire.Prove(ctx,tptp)
  if err!=nil { return fmt.Errorf("Prove(%q): %v",*caseName,err) }
  log.Printf("out = %v",out)
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
