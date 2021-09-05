package main

import (
  "context"
  "log"
  "strconv"
  "fmt"
  "flag"
  "time"
  "os"
  "sort"
  "strings"
  "path"
  "io/ioutil"

  "github.com/golang/protobuf/proto"
  "google.golang.org/protobuf/types/known/durationpb"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/problems"
  mcts "github.com/pompon0/tptp_benchmark_go/ffprover"
  mpb "github.com/pompon0/tptp_benchmark_go/ffprover/mcts_go_proto"
)

const mcts_bin_path = "__main__/ffprover/mcts"

var timeout = flag.Duration("timeout",time.Minute,"")
var modelsPath = flag.String("models_path","","")
var caseNamePrefix = flag.String("case_name_prefix","","")
var outputDir = flag.String("output_dir","","")
var solvedInReportPath = flag.String("solved_in_report_path","","")
var fullSearch = flag.Bool("full_search",false,"")
var featuresSpaceSize = flag.Int("features_space_size",1<<15,"")

type namedProblem struct { name string; p *problems.Problem }

func process(ctx context.Context, p namedProblem) error {
  outDir,err:=NewDir(*outputDir,"")
  if err!=nil { return fmt.Errorf("NewDir(): %w",err) }

  tptpFOF,err := p.p.Get()
  if err!=nil {
    return fmt.Errorf("p.Get(): %w",err)
  }
  if has,err := tool.TptpHasEquality(ctx,tptpFOF); err!=nil {
    return fmt.Errorf("tool.TptpHasEquality(): %w",err)
  } else if !has {
    return nil
  }
  log.Printf("solving %q",p.name)

  tptpCNF,err := eprover.FOFToCNF(ctx,tptpFOF)
  if err!=nil { return fmt.Errorf("eprover.FOFToCNF(): %w",err) }
  cnf,err := tool.TptpToProto(ctx,tool.CNF,tptpCNF)
  if err!=nil { return fmt.Errorf("tool.TptpToProto(): %w",err) }

  input := &mpb.Input{
    Problem: cnf,
    Timeout: durationpb.New(*timeout),
    FullSearch: *fullSearch,
    FeaturesSpaceSize: uint64(*featuresSpaceSize),
    PlayoutsPerBigstep: 100,
    PlayoutDepth: 10,
  }
  if *modelsPath!="" {
    modelsBytes,err := ioutil.ReadFile(*modelsPath)
    if err!=nil { return fmt.Errorf("io.ReadFile(): %w",err) }
    input.Models = &mpb.ModelSet{}
    if err:=proto.Unmarshal(modelsBytes,input.Models); err!=nil {
      return fmt.Errorf("proto.Unmarshal(): %w",err)
    }
  }

  gracefulExitTimeout := time.Minute
  cmdCtx,cancel := context.WithTimeout(ctx,*timeout+gracefulExitTimeout)
  defer cancel()
  output,err := mcts.MCTS(cmdCtx,input)
  if err!=nil {
    return fmt.Errorf("MCTS(): %w",err)
  }

  // Print status & stats
  fmt.Printf("Status %v\n",output.GetStatus())
  fmt.Printf("%v\n\n",output.GetStats())

  // Print priofiler data.
  prof := output.GetProfiler().GetEntries()
  sort.Slice(prof,func(i,j int) bool { return prof[i].GetTimeS() > prof[j].GetTimeS() })
  for _,e := range prof { log.Printf("%v",e) }

  // Save output data.
  problemName := strings.ReplaceAll(p.name,"/","_")

  outputRaw,err := proto.Marshal(output)
  if err!=nil {
    return fmt.Errorf("proto.Marshal(): %w",err)
  }
  if err:=ioutil.WriteFile(outDir.File(problemName),outputRaw,0777); err!=nil {
    return fmt.Errorf("ioutil.WriteFile(): %w",err)
  }
  return nil
}

type Dir struct {
  root string
  path string
}

func NewDir(root string, path_ string) (Dir,error) {
  rootStat,err := os.Stat(root)
  if err!=nil { return Dir{},fmt.Errorf("os.Stat(%q): %w",root,err) }
  if !rootStat.IsDir() {
    return Dir{},fmt.Errorf("%q is not a dir",root)
  }
  if err:=os.MkdirAll(path.Join(root,path_),0777); err!=nil {
    return Dir{},fmt.Errorf("os.MkdirAll(): %w",err)
  }
  return Dir{root,path_},nil
}

func (d Dir) File(name string) string {
  return path.Join(d.root,d.path,name)
}

func sgeTaskID() (int,bool) {
  id,err := strconv.Atoi(os.Getenv("SGE_TASK_ID"))
  if err!=nil {
    return 0,false
  }
  return id,true
}

func run(ctx context.Context) error {
  if *outputDir=="" {
    return fmt.Errorf("output_dir is required")
  }
  filter := func(p namedProblem) bool {
    if !strings.HasPrefix(p.name,*caseNamePrefix) { return false }
    // if problems.SolvedByVampireWithoutEquality[p.name] { return false }
    return true
  }
  if p := *solvedInReportPath; p!="" {
    report,err := problems.ReadReport(*solvedInReportPath)
    if err!=nil {
      return fmt.Errorf("problems.ReadReport(): %w",err)
    }
    m := map[string]bool{}
    for _,c := range report.Cases {
      m[c.Name] = c.GetOutput().GetSolved()
    }
    f := filter
    filter = func(p namedProblem) bool { return m[p.name] && f(p) }
  }

  mp,cancel,err := problems.MizarProblems()
  if err!=nil { return fmt.Errorf("problems.MizarProblems(): %w",err) }
  defer cancel()

  /*tp,cancel,err := problems.TptpProblems()
  if err!=nil { return fmt.Errorf("problems.TptpProblems(): %w",err) }
  defer cancel()*/

  var ps []namedProblem
  for name,p := range mp {
    ps = append(ps,namedProblem{name,p})
  }
  /*for name,p := range tp {
    ps = append(ps,namedProblem{name,p})
  }*/
  // Fix the order for determinism.
  sort.Slice(ps,func(i,j int)bool{ return ps[i].name<ps[j].name })

  id,ok := sgeTaskID()
  total := 0
  for _,p := range ps {
    if !filter(p) { continue }
    if ok && total==id {
      if err:=process(ctx,p); err!=nil {
        return fmt.Errorf("process(%q): %w",p.name,err)
      }
    }
    total++
  }
  if !ok {
    log.Printf("total = %d",total)
  }
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
