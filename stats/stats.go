package main

import(
  "fmt"
  "log"
  "flag"
  "context"
  "os"
  "strings"
  //"sort"

  "github.com/pompon0/tptp_benchmark_go/problems"
  "github.com/pompon0/tptp_benchmark_go/tool"
  spb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/solutions_go_proto"
)

var proofDir = flag.String("proof_dir","","")

var dim = []string{"refl","symm","trans","pred","fun","cla"}

const reflDim = 1<<0
const symmDim = 1<<1
const transDim = 1<<2
const predDim = 1<<3
const funDim = 1<<4
const claDim = 1<<5

type Profile int

func bool2int(b bool) int {
  if b { return 1 }
  return 0
}

func profile(stats *spb.Stats) Profile {
  res := 0
  if len(stats.OrClauses)>0 { res |= claDim }
  if len(stats.FunMono)>0 { res |= funDim }
  if len(stats.PredMono)>0 { res |= predDim }
  if stats.Trans>0 { res |= transDim }
  if stats.Symm>0 { res |= symmDim }
  if stats.Refl>0 { res |= reflDim }
  return Profile(res)
}

func countAxioms(s *spb.Stats) int {
  c := s.Refl + s.Symm + s.Trans
  for _,a := range s.PredMono { c += a.Count }
  for _,a := range s.FunMono { c += a.Count }
  return int(c)
}

func countClauses(s *spb.Stats) int {
  c := countAxioms(s)
  for _,x := range s.OrClauses { c += int(x.Count) }
  return c
}

func (p Profile) String() string {
  s := []string{}
  for i,d := range dim {
    if p>>uint(i)&1==1 { s = append(s,d) }
  }
  return strings.Join(s,"|")
}

func run(ctx context.Context) error {
  if _,err := os.Stat(*proofDir); os.IsNotExist(err) {
    return fmt.Errorf("%q doesn't exist",*proofDir)
  }
  profileCount := map[Profile]int{}
  byAxiomCount := map[string]int{}
  percentiles := map[int]int{}
  prob,err := problems.GetProblems(ctx)
  if err!=nil { return fmt.Errorf("problems.GetProblems(): %v",err) }
  for name,_ := range prob {
    proofs,err := problems.ReadProofs(*proofDir,name)
    if err!=nil { return fmt.Errorf("problems.ReadProofs(%q): %v",name,err) }
    if len(proofs)>0 {
      stats,err := tool.ValidateProof(ctx,proofs[0])
      if err!=nil {
        return fmt.Errorf("problems.ReadProofs(%q): %v",name,err)
      }
      ratio := float64(countAxioms(stats))/float64(countClauses(stats))
      for p:=0; p<=100; p += 5 {
        if float64(p)<=ratio*100. {
          percentiles[p] += 1
        }
      }
      profileCount[profile(stats)] += 1
      byAxiomCount[fmt.Sprintf("%02d/%02d",countClauses(stats),countAxioms(stats))] += 1
    }
  }
  for p:=0; p<=100; p+=5 {
    log.Printf(">=%d%% : %d",p,percentiles[p])
  }
  /*
  var profileCountKeys []Profile
  for p,_ := range profileCount { profileCountKeys = append(profileCountKeys,p) }
  sort.Slice(profileCountKeys, func(a,b int) bool { return profileCount[profileCountKeys[a]]>profileCount[profileCountKeys[b]] })
  for _,p := range profileCountKeys { log.Printf("%v: %d",p,profileCount[p]) }

  var byAxiomCountKeys []string
  for k,_ := range byAxiomCount { byAxiomCountKeys = append(byAxiomCountKeys,k) }
  sort.Strings(byAxiomCountKeys)
  for _,c := range byAxiomCountKeys { log.Printf("%s: %d",c,byAxiomCount[c]) }*/
  return nil
}

func main() {
  flag.Parse()
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
