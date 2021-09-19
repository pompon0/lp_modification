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
  "io/ioutil"

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

var renameRegexp = regexp.MustCompile("^(fof|cnf)\\(([^,\\\"\\']*|\\'[^\\'\\\\]*\\')(,.*)$")
var commentRegexp = regexp.MustCompile("(?s)//.*?\n|/\\*.*?\\*/")
var unwantedRegexp = regexp.MustCompile("\\\"|\\W\\d|\\s\\d|^\\d")

var excludedProblems = map[string]bool{
 // contain numbers
 "CSR/CSR117+1.p": true,
 "LCL/LCL888+1.p": true,
 "LCL/LCL889+1.p": true,
 "LCL/LCL890+1.p": true,
 "LCL/LCL891+1.p": true,
 "LCL/LCL892+1.p": true,
 "LCL/LCL893+1.p": true,
 "LCL/LCL894+1.p": true,
 "LCL/LCL895+1.p": true,
 "LCL/LCL896+1.p": true,
 "LCL/LCL897+1.p": true,
 "LCL/LCL898+1.p": true,
 "LCL/LCL899+1.p": true,
 "LCL/LCL900+1.p": true,
 "LCL/LCL901+1.p": true,
 "LCL/LCL902+1.p": true,
 "LCL/LCL903+1.p": true,
 // contain distinct objects
 "SYO/SYO561+1.p": true,
}

func inlineImports(ctx context.Context, rootPath string, problemPath string, maxOutput int) ([]byte,error) {
  outBuf := NewBoundedBuffer(maxOutput)
  var inBuf bytes.Buffer
  cmd := exec.CommandContext(ctx,utils.Runfile(tptp4XBinPath),"-x","-umachine","-ftptp",problemPath)
  cmd.Dir = rootPath
  cmd.Stdin = &inBuf
  cmd.Stdout = outBuf
  cmd.Stderr = os.Stderr
  if err := cmd.Run(); err!=nil {
    if ctx.Err()!=nil { return nil,ctx.Err() }
    if outBuf.Err()!=nil { return nil,outBuf.Err(); }
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }
  // tptp4X happily produces duplicate lines, but then complains when reading 
  // them back.
  var lines []string
  for i,l := range strings.Split(commentRegexp.ReplaceAllString(outBuf.String(),""),"\n") {
    if !strings.HasPrefix(l,"%") {
      lines = append(lines,renameRegexp.ReplaceAllString(l,fmt.Sprintf("${1}(l%d${3}",i)))
    }
  }
  return []byte(strings.Join(lines,"\n")),nil
}

// due to a bug in tptp4X, you cannot inline and shorten in one step.
func shorten(ctx context.Context, tptp []byte) ([]byte,error) {
  var inBuf,outBuf bytes.Buffer
  if _,err := inBuf.Write(tptp); err!=nil {
    return nil,fmt.Errorf("inBuf.Write(): %v",err)
  }
  cmd := exec.CommandContext(ctx,utils.Runfile(tptp4XBinPath),"-umachine","-ftptp","-tshorten","--")
  cmd.Stdin = &inBuf
  cmd.Stdout = &outBuf
  cmd.Stderr = os.Stderr
  if err := cmd.Run(); err!=nil {
    if ctx.Err()!=nil { return nil,ctx.Err() }
    log.Printf("in = %v",string(tptp))
    log.Printf("out = %v",outBuf.String())
    return nil,fmt.Errorf("cmd.Run(): %v",err)
  }
  return outBuf.Bytes(),nil
}

const statusTheorem = "Theorem"
const statusContradictoryAxioms = "ContradictoryAxioms"
const statusUnsatisfiable = "Unsatisfiable"
const statusCounterSatisfiable = "CounterSatisfiable"
const statusSatisfiable = "Satisfiable"
const statusUnknown = "Unknown"
const statusOpen = "Open"

func isTheorem(problemPath string) (bool,error) {
  data,err := ioutil.ReadFile(problemPath)
  if err!=nil { return false,fmt.Errorf("ioutil.ReadFile(%q): %v",problemPath,err) }
  for _,l := range strings.Split(string(data),"\n") {
    l := strings.ReplaceAll(l," ","")
    const statusPrefix = "%Status:"
    if strings.HasPrefix(l,statusPrefix) {
      switch status := strings.TrimPrefix(l,statusPrefix); status {
        case statusTheorem: return true,nil
        case statusContradictoryAxioms: return true,nil
        case statusUnsatisfiable: return true,nil
        case statusCounterSatisfiable: return false,nil
        case statusSatisfiable: return false,nil
        case statusUnknown: return false,nil
        case statusOpen: return false,nil
        default: return false,fmt.Errorf("unexpected status %q",status)
      }
    }
  }
  return false,fmt.Errorf("problem status not found")
}

//////////////////////////////////////

const inlineImportsMaxOutput = 1*1024*1024
const eproverCNFTimeout = 3*time.Second
const toProtoTimeout = 3*time.Second


var fofFileRegexp = regexp.MustCompile("^.*\\+.*\\.p$")
const problemsDir = "Problems"
const outputZipName = "tptp_problems.zip"

var tptpRoot = flag.String("tptp_root","","")
var shortenNames = flag.Bool("shorten_names",false,"")

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

    // skip if excluded
    if excludedProblems[relPath] {
      log.Printf("Skipping %q; excluded",relPath)
      return nil
    }

    // skip if too large
    if f.Size()>inlineImportsMaxOutput {
      log.Printf("Skipping %q; file too large",relPath)
      return nil
    }

    // skip if not a theorem
    if ok,err := isTheorem(p); err!=nil {
      log.Printf("isTheorem(%q): %v",relPath,err)
      return fmt.Errorf("isTheorem(%q): %v",relPath,err)
    } else if !ok {
      log.Printf("Skipping %q; not a theorem",relPath)
      return nil
    }

    // inline imports or skip
    tptpFOF,err := inlineImports(ctx,*tptpRoot,p,inlineImportsMaxOutput)
    if err==errBufferFull {
      log.Printf("Skipping %q; inlineImports(): %v",relPath,err)
      return nil
    } else if err!=nil {
      return fmt.Errorf("inlineImports(%q): %v",relPath,err)
    }
    if *shortenNames {
      if tptpFOF,err = shorten(ctx,tptpFOF); err!=nil {
        return fmt.Errorf("shorten(%q): %v",relPath,err)
      }
    }
    log.Printf("len(tptpFOF) = %v",len(tptpFOF))

    // convert to CNF or skip
    eproverCtx,cancel := context.WithTimeout(ctx,eproverCNFTimeout)
    defer cancel()
    cnf,err := eprover.FOFToCNF(eproverCtx,&tool.TPTP{relPath,tptpFOF})
    if err==context.DeadlineExceeded {
      log.Printf("Skipping %q; eprover.FOFToCNF(): %v",relPath,err)
      return nil
    } else if err!=nil {
      return fmt.Errorf("eprover.FOFToCNF(%q): %v",relPath,err)
    }
    log.Printf("len(tptpCNF) = %v",len(cnf.Raw))

    // convert to proto or die
    toProtoCtx,cancel := context.WithTimeout(ctx,toProtoTimeout)
    defer cancel()
    tptpProto,err := cnf.ToProto(toProtoCtx,tool.CNF)
    if err!=nil {
      return fmt.Errorf("tool.TptpToProto(%q): %v",relPath,err)
    }
    tptpProtoRaw,err := proto.Marshal(tptpProto)
    if err!=nil { return fmt.Errorf("proto.Marshal(): %v") }
    log.Printf("len(tptpProto) = %v",len(tptpProtoRaw))

    if unwantedRegexp.Find(tptpFOF)!=nil {
      return fmt.Errorf("unwanted problem %q",relPath)
    }

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
