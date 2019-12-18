package main

import (
  "fmt"
  "bufio"
  "context"
  "time"
  "os"
  "os/exec"
  "bytes"
  "flag"
  "regexp"
  "strings"
  "errors"
  "log"
  "path"
  "path/filepath"
  "archive/zip"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/utils"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/tool"
)

const tptp4XBinPath = "tptp4X/file/downloaded"

var errBufferFull = errors.New("Buffer full")

type BoundedBuffer struct {
  bound int
  buf bytes.Buffer
  err error
}

func NewBoundedBuffer(bound int) *BoundedBuffer {
  return &BoundedBuffer{bound: bound}
}

func (b *BoundedBuffer) Err() error { return b.err }

func (b *BoundedBuffer) Write(p []byte) (int,error) {
  if b.buf.Len()+len(p)>b.bound {
    b.err = errBufferFull
    return 0,b.Err()
  }
  //log.Printf("Write() len = %d + %d",b.buf.Len(),len(p))
  return b.buf.Write(p)
}

func (b *BoundedBuffer) String() string { return b.buf.String() }

func inlineImports(ctx context.Context, rootPath string, problemPath string, maxOutput int) ([]byte,error) {
  outBuf := NewBoundedBuffer(maxOutput)
  var inBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(tptp4XBinPath),"-x","-umachine","-ftptp","-tshorten",problemPath)
  cmd.Dir = rootPath
  cmd.Stdin = &inBuf
  cmd.Stdout = outBuf
  cmd.Stderr = os.Stderr
  if err := cmd.Run(); err!=nil {
    if ctx.Err()!=nil { return nil,ctx.Err() }
    if outBuf.Err()!=nil { return nil,outBuf.Err(); }
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }
  var lines []string
  for _,l := range strings.Split(outBuf.String(),"\n") {
    if !strings.HasPrefix(l,"%") {
      lines = append(lines,l+"\n")
    }
  }
  return []byte(strings.Join(lines,"")),nil
}

//////////////////////////////////////

const inlineImportsMaxOutput = 1*1024*1024
const eproverCNFTimeout = 3*time.Second
const toProtoTimeout = 3*time.Second


var fofFileRegexp = regexp.MustCompile("^.*\\+.*\\.p$")
const problemsDir = "Problems"
const outputZipName = "tptp_problems.zip"

var tptpRoot = flag.String("tptp_root","","")


func run(ctx context.Context) error {
  problemsPath := path.Join(*tptpRoot,problemsDir)
  if _,err := os.Stat(problemsPath); os.IsNotExist(err) {
    return fmt.Errorf("%q doesn't exist",problemsPath)
  }
  zipPath := path.Join(*tptpRoot,outputZipName)
  zipFile,err := os.Create(zipPath)
  if err!=nil {
    return fmt.Errorf("os.Create(%q): %v",zipPath,err)
  }
  zipWriter := zip.NewWriter(bufio.NewWriter(zipFile))
  defer zipWriter.Close()
  log.Printf("%q",problemsPath)
  return filepath.Walk(problemsPath,func(p string, f os.FileInfo, err error) error {
    if err!=nil { return err }
    if f.IsDir() { return nil }
    relPath,err := filepath.Rel(problemsPath,p)
    if err!=nil { return fmt.Errorf("filepath.Rel(%q,%q): %v",problemsPath,p,err) }
    if !fofFileRegexp.MatchString(relPath) { return nil }

    // skip if too large
    if f.Size()>inlineImportsMaxOutput {
      log.Printf("Skipping %q; file too large",relPath)
      return nil
    }

    // inline imports or skip
    // inlineImportsCtx,cancel := context.WithTimeout(ctx,inlineImportsTimeout)
    // defer cancel()
    tptpFOF,err := inlineImports(ctx,*tptpRoot,p,inlineImportsMaxOutput)
    if err==errBufferFull {
      log.Printf("Skipping %q; inlineImports(): %v",relPath,err)
      return nil
    } else if err!=nil {
      return fmt.Errorf("inlineImports(%q): %v",relPath,err)
    }
    log.Printf("len(tptpFOF) = %v",len(tptpFOF))

    // convert to CNF or skip
    eproverCtx,cancel := context.WithTimeout(ctx,eproverCNFTimeout)
    defer cancel()
    tptpCNF,err := eprover.FOFToCNF(eproverCtx,tptpFOF)
    if err==context.DeadlineExceeded {
      log.Printf("Skipping %q; eprover.FOFToCNF(): %v",relPath,err)
      return nil
    } else if err!=nil {
      return fmt.Errorf("eprover.FOFToCNF(%q): %v",relPath,err)
    }
    log.Printf("len(tptpCNF) = %v",len(tptpCNF))

    // convert to proto or die
    toProtoCtx,cancel := context.WithTimeout(ctx,toProtoTimeout)
    defer cancel()
    tptpProto,err := tool.TptpToProto(toProtoCtx,tool.CNF,tptpCNF)
    if err!=nil {
      return fmt.Errorf("tool.TptpToProto(%q): %v",relPath,err)
    }
    tptpProtoRaw,err := proto.Marshal(tptpProto)
    if err!=nil { return fmt.Errorf("proto.Marshal(): %v") }
    log.Printf("len(tptpProto) = %v",len(tptpProtoRaw))

    // append problem to zip
    log.Printf("appending %q\n",relPath)
    w,err := zipWriter.Create(relPath)
    if err!=nil {
      return fmt.Errorf("zipWriter.Create(%q): %v",relPath,err)
    }
    if _,err := w.Write(tptpFOF); err!=nil {
      return fmt.Errorf("w.Write(%q): %v",p,err)
    }
    return nil
  })
}

func main() {
  flag.Parse()
  if err := run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
