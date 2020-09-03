package sample

import (
  "context"
  "testing"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

func TestSampleProblems(t *testing.T) {
  ctx := context.Background()
  for n,p := range SampleProblems() {
    n,p := n,p
    t.Run(n, func(t *testing.T) {
      t.Parallel()
      if _,err := tool.TptpToProto(ctx,tool.FOF,[]byte(p)); err!=nil {
        t.Fatalf("tool.TptpToProto(): %v",err)
      }
    })
  }
}
