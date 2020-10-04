package main

import (
  "context"
  "io/ioutil"
  "log"
  "fmt"
  "os"

  "github.com/golang/protobuf/proto"
  "github.com/pompon0/tptp_benchmark_go/tool"
  "github.com/pompon0/tptp_benchmark_go/eprover"
  tpb "github.com/pompon0/tptp_benchmark_go/tool/bin/bin_go_proto"
  xpb "github.com/pompon0/tptp_benchmark_go/tptp_parser/proto/tptp_go_proto"
)

func run(ctx context.Context) error {
  raw,err := ioutil.ReadAll(os.Stdin)
  if err!=nil { return fmt.Errorf("ioutil.ReadAll(): %w",err) }
  var req tpb.Request
  if err:=proto.Unmarshal(raw,&req); err!=nil {
    return fmt.Errorf("proto.Unmarshal(): %w",err)
  }

  resp := &tpb.Response{}
  // ProtoToTptp
  if req := req.GetProtoToTptp(); req!=nil {
    tptp,err := tool.ProtoToTptp(ctx,req.GetFile())
    if err!=nil { return fmt.Errorf("tool.ProtoToTptp(): %w",err) }
    resp.ProtoToTptp = &tpb.ProtoToTptpResp{ Tptp: string(tptp) }
  }
  // TptpToProto
  if req := req.GetTptpToProto(); req!=nil {
    var lang tool.Language
    switch req.GetLang() {
    case xpb.Input_FOF: lang = tool.FOF
    case xpb.Input_CNF: lang = tool.CNF
    default: return fmt.Errorf("unknown language: %v",req.GetLang())
    }
    f,err := tool.TptpToProto(ctx,lang,[]byte(req.GetTptp()))
    if err!=nil { return fmt.Errorf("tool.TptpToProto(): %w",err) }
    resp.TptpToProto = &tpb.TptpToProtoResp{ File: f }
  }
  // FOFToCNF
  if req := req.GetFofToCnf(); req!=nil {
    cnf,err := eprover.FOFToCNF(ctx,[]byte(req.GetTptpFof()))
    if err!=nil { return fmt.Errorf("eprover.FOFToCNF(): %w",err) }
    resp.FofToCnf = &tpb.FofToCnfResp{ TptpCnf: string(cnf) }
  }

  rawResp,err := proto.Marshal(resp)
  if err!=nil { return fmt.Errorf("proto.Marshal(): %w",err) }
  if _,err:=os.Stdout.Write(rawResp); err!=nil {
    return fmt.Errorf("os.Stdout.Write: %w",err)
  }
  return nil
}

func main() {
  if err:=run(context.Background()); err!=nil {
    log.Fatalf("run(): %v",err)
  }
}
