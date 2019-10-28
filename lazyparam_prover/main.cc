//#define DEBUG
//#define VERBOSE
//#define PROFILE

#include <iostream>
#include "lazyparam_prover/tableau.h"
#include "lazyparam_prover/pred.h"
#include "solutions.pb.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"

ABSL_FLAG(uint64_t,proof_size_limit,200,"maximal size of a proof to search for");

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  absl::ParseCommandLine(argc, argv);

  str file_raw((std::istreambuf_iterator<char>(std::cin)), (std::istreambuf_iterator<char>()));
  ParseCtx ctx;
  OrForm f(ctx.parse_notAndForm(file_raw));

  auto proof = prove_loop(f,absl::GetFlag(FLAGS_proof_size_limit));
  std::cerr << profile.show(); 
  if(!proof) return 1;
  ProtoCtx pctx(ctx);
  OrForm proof_form;
  for(auto cla : proof->source) proof_form.and_clauses.push_back(DerAndClause(1,cla));
  solutions::CNF res;
  *res.mutable_proof() = pctx.proto_notAndForm(NotAndForm(proof_form));
  std::cout << res.DebugString() << std::endl;
  return 0;
}
