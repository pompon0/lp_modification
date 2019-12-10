package problems

/*import (
  "os"
  "fmt"
  "context"
  "path"
  "path/filepath"
  "io/ioutil"
  "compress/gzip"
  "archive/tar"

  "github.com/pompon0/tptp_benchmark_go/utils"
)*/

/*func GetProblems(ctx context.Context) (map[string][]byte,error) {
  problems := map[string][]byte{}
  if err := filepath.Walk(utils.Runfile(problemsPath),func(p string, f os.FileInfo, err error) error {
    if err!=nil { return err }
    if f.IsDir() { return nil }
    tptp,err := ioutil.ReadFile(p)
    if err!=nil { return fmt.Errorf("ioutil.ReadFile(%q): %v",p,err) }
    problems[path.Base(p)] = tptp
    return nil
  }); err!=nil { return nil,err }
  if got,want := len(problems),problemsCount; got!=want {
    return nil,fmt.Errorf("len(problems) = %v, want %v",got,want)
  }
  return problems,nil
}

func GetProblems(ctx context.Context) (map[string][]byte,error) {
  req,err := http.NewRequest("GET",problemsUrl,nil)
  if err!=nil { return nil,fmt.Errorf("http.NewRequest(): %v",err) }
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

func WriteProblemSet(problems map[string][]byte, filepath string) error {
  f,err := os.Create(filepath)
  defer f.Close()
  if err!=nil { return fmt.Errorf("os.Create(): %v",err) }
  gzipWriter := gzip.NewWriter(f)
  defer gzipWriter.Close()
  tarWriter := tar.NewWriter(gzipWriter)
  defer tarWriter.Close()
  for k,v := range problems {
    if err := tarWriter.WriteHeader(&tar.Header{
      Typeflag: tar.TypeReg,
      Name: k,
      Size: int64(len(v)),
      Mode: 0666,
    }); err!=nil { return fmt.Errorf("tarWriter.WriteHeader(): %v",err) }
    if _,err:=tarWriter.Write(v); err!=nil { return fmt.Errorf("tarWriter.Write(): %v",err) }
  }
  return nil
}*/
