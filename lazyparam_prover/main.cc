//#define DEBUG
//#define VERBOSE
//#define PROFILE

#include <csignal>
#include <iostream>
#include "lazyparam_prover/tableau.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/ctx.h"
#include "lazyparam_prover/log.h"
#include "solutions.pb.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/time.h"

ABSL_FLAG(absl::Duration,timeout,absl::Seconds(4),"spend timeout+eps time on searching");

using namespace tableau;

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);

  str file_raw((std::istreambuf_iterator<char>(std::cin)), (std::istreambuf_iterator<char>()));
  ParseCtx parse_ctx;
  OrForm f(parse_ctx.parse_notAndForm(file_raw));

  
  auto ctx = Ctx::with_timeout(absl::GetFlag(FLAGS_timeout));
  auto out = prove_loop(*ctx,f);
  std::cerr << profile.show(); 
  
  solutions::ProverOutput outProto;
  outProto.set_cost(out.cost);
  outProto.set_continuation_count(out.cont_count);
  if(out.proof){
    outProto.set_solved(true);
    ProtoCtx pctx(parse_ctx);
    OrForm proof_form;
    for(auto cla : out.proof->source) proof_form.and_clauses.push_back(DerAndClause(1,cla));
    *outProto.mutable_proof() = pctx.proto_notAndForm(NotAndForm(proof_form));
  }
  if(!outProto.SerializeToOstream(&std::cout)) {
    error("outProto.SerializeToOstream() failed");  
  }
  return 0;
}
