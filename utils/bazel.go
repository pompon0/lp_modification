package utils

import (
  "os"
  "path"
  "path/filepath"
  "strings"
  "log"
)

func Runfile(workspacePath string) string {
  p := path.Join(RunfilesRoot(),workspacePath)
  if _,err := os.Stat(p); os.IsNotExist(err) {
    ShowRunfiles()
    log.Fatalf("runfile %q doesn't exist",p)
  }
  return p
}

func RunfilesRoot() string {
  if dir := os.Getenv("RUNFILES_DIR"); dir!="" { return dir }
  wd,err := os.Getwd()
  if err!=nil { log.Fatalf("os.Getwd(): %v",err) }
  return wd
}

func ShowRunfiles() {
  err := filepath.Walk(RunfilesRoot(), func(path string, f os.FileInfo, err error) error {
    log.Printf("%q",path)
    return nil
  })
  if err!=nil { log.Fatalf("filepath.Walk() = %v",err) }
}

var BazelVars = map[string]bool {
  "RUNFILES": true,
  "BUILD_WORKING_DIRECTORY": true,
  "BUILD_WORKSPACE_DIRECTORY": true,
  "BAZEL_REAL": true,
}

func ShowEnv() {
  for _,v := range os.Environ() {
    k := strings.Split(v,"=")[0]
    if BazelVars[k] {
      log.Printf("%q",v)
    } else {
      log.Printf("%q",k)
    }
  }
}
