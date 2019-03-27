package main

import (
  "fmt"
  "os"
  "os/exec"
  "time"
  "log"
  "context"
  "net/http"
  "io"
  "io/ioutil"
  "compress/gzip"
  "archive/tar"
  "strings"

  "golang.org/x/sync/errgroup"
)

const problemsUrl = "https://storage.googleapis.com/tptp/tptp_sample.tgz";

func getProblems(ctx context.Context) (map[string][]byte,error) {
  req,err := http.NewRequest("GET",problemsUrl,nil)
  if err!=nil { return nil,fmt.Errorf("http.NewReuqest(): %v",err) }
  resp,err := http.DefaultClient.Do(req.WithContext(ctx))
  defer resp.Body.Close()

  gzipReader,err := gzip.NewReader(resp.Body)
  if err!=nil { return nil,fmt.Errorf("gzip.NewReader(): %v",err) }
  tarReader := tar.NewReader(gzipReader)
  problems := map[string][]byte{}
  for {
    h,err := tarReader.Next()
    if err==io.EOF { break }
    if err!=nil { return nil,fmt.Errorf("tarReader.Next(): %v",err) }
    problems[h.Name],err = ioutil.ReadAll(tarReader)
    if err!=nil { return nil,fmt.Errorf("ioutil.ReadAll(): %v",err) }
  }
  return problems,nil
}

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
  proverPath string,
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
        output,err := exec.CommandContext(cmdCtx,proverPath,proverPath,file.Name()).Output()
        results <- Result{p.name,output,err}
        return nil
      }(); err!=nil { return err }
    }
  }
}

func run(ctx context.Context, timeout time.Duration, cores int) error {
  if len(os.Args)!=2 { return fmt.Errorf("usage: %s <path to prover binary>",os.Args[0]) }
  proverPath := os.Args[1]
  log.Printf("using %q as a prover",proverPath)

  problems,err := getProblems(ctx)
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
    return worker(gCtx,timeout,proverPath,problemsChan,resultsChan)
  })}

  group.Go(func() error {
    okCount := map[string]int{}
    errCount := 0
    for i:=0; i<len(problems); i++ {
      select {
      case <-gCtx.Done(): return nil
      case r := <-resultsChan:
        if r.err!=nil { errCount++ } else {
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
