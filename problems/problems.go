package problems

import (
  "fmt"
  "io/ioutil"
  "archive/zip"
  "strings"

  "github.com/pompon0/tptp_benchmark_go/utils"
)

const tptpProblemsPath = "tptp_problems/file/downloaded"
const tptpProblemsCount = 8630

func TptpProblems() (map[string]*Problem,func(),error) {
  return openZip(utils.Runfile(tptpProblemsPath))
}

const mizarProblemsPath = "mizar_problems/file/downloaded"
const mizarProblemsCount = 2003

func MizarProblems() (map[string]*Problem,func(),error) {
  return openZip(utils.Runfile(mizarProblemsPath))
}

type Problem struct {
  file *zip.File
}

func (p *Problem) Get() ([]byte,error) {
  r,err := p.file.Open()
  if err!=nil { return nil,fmt.Errorf("p.file.Open(): %v",err) }
  defer r.Close()
  data,err := ioutil.ReadAll(r)
  if err!=nil { return nil,fmt.Errorf("ioutil.ReadAll(): %v",err) }
  return data,nil
}

// zip format is preferred over tar.gz, because it provides random access to files.
// if zip (per-file) compression is insufficient, consider switching to LZMA.
func openZip(path string) (map[string]*Problem,func(),error) {
  r,err := zip.OpenReader(path)
  if err != nil { return nil,nil,fmt.Errorf("zip.OpenReader(): %v",err) }
  files := map[string]*Problem{}
  for _,f := range r.File {
    if !strings.HasSuffix(f.Name,"/") {
      // f is not a directory
      files[f.Name] = &Problem{f}
    }
  }
  return files,func(){ r.Close() },nil
}
