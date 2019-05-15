package main

import (
  "fmt"
  "bytes"
  "os"
  "os/exec"
  "time"
  "log"
  "context"
  "io/ioutil"
  "strings"

  "golang.org/x/sync/errgroup"
  "github.com/pompon0/tptp_benchmark_go/problems"
)

type Problem struct {
  name string
  data []byte
}

type Result struct {
  name string
  output []byte
  err error
}

func worker(
  ctx context.Context,
  timeout time.Duration,
  proverArgv []string,
  problems <-chan Problem,
  results chan<- Result,
) error {
  for {
    select {
    case <-ctx.Done(): return nil
    case p,ok := <-problems:
      if !ok { return nil }
      if err := func() error {
        file,err := ioutil.TempFile("","tptp_benchmark_")
        if err!=nil { return fmt.Errorf("ioutil.TempFile(): %v",err) }
        defer os.Remove(file.Name())
        if _,err := file.Write(p.data); err!=nil { return fmt.Errorf("file.Write(): %v",err) }
        if err:=file.Close(); err!=nil { return fmt.Errorf("file.Close(): %v",err) }
        cmdCtx,cancel := context.WithTimeout(ctx,timeout)
        defer cancel()
        var outBuf,errBuf bytes.Buffer
        cmd := exec.CommandContext(cmdCtx,proverArgv[0],append(proverArgv[1:],file.Name())...)
        cmd.Stdout = &outBuf
        cmd.Stderr = &errBuf
        err = cmd.Run()
        results <- Result{p.name,outBuf.Bytes(),err}
        return nil
      }(); err!=nil { return err }
    }
  }
}

func run(ctx context.Context, timeout time.Duration, cores int) error {
  if len(os.Args)<2 { return fmt.Errorf("usage: %s <path to prover binary> <flags>...",os.Args[0]) }
  proverArgv := os.Args[1:]
  log.Printf("using %v as a prover",proverArgv)

  problems,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("getProblems(): %v",err) }

  problemsChan := make(chan Problem,5)
  resultsChan := make(chan Result,5)
  group,gCtx := errgroup.WithContext(ctx)

  group.Go(func() error {
    for name,data := range problems {
      select {
      case <-gCtx.Done(): return nil
      case problemsChan <- Problem{name,data}:
      }
    }
    close(problemsChan)
    return nil
  })

  for i:=0; i<cores; i++ { group.Go(func() error {
    return worker(gCtx,timeout,proverArgv,problemsChan,resultsChan)
  })}

  group.Go(func() error {
    okCount := map[string]int{}
    errCount := 0
    for i:=0; i<len(problems); i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r.err!=nil {
          errCount++
          //log.Printf("error = %q",r.err)
        } else {
          lines := strings.Split(strings.TrimSpace(string(r.output)),"\n")
          last := lines[len(lines)-1]
          okCount[last]++
        }
        log.Printf("done %v/%v err=%v %v",i+1,len(problems),errCount,okCount)
        //log.Printf("%q :: %v :: %s",r.name,r.err,r.output)
      }
    }
    return nil
  })

  if err := group.Wait(); err!=nil {
    return fmt.Errorf("group.Wait(); %v",err)
  }

  return nil
}

func main() {
  if err := run(context.Background(),2*time.Second,4); err!=nil {
    log.Fatalf("%v",err)
  }
}
