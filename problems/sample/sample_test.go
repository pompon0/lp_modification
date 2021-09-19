package sample

import (
  "context"
  "testing"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

func TestSampleProblems(t *testing.T) {
  ctx := context.Background()
  for _,p := range SampleProblems() {
    p := p
    t.Run(p.Name, func(t *testing.T) {
      t.Parallel()
      if _,err := p.ToProto(ctx,tool.FOF); err!=nil {
        t.Fatalf("p[%q].ToProto(): %v",p.Name,err)
      }
    })
  }
}
