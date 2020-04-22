//#define DEBUG
//#define VERBOSE
//#define PROFILE

#include <csignal>
#include <iostream>
#include "lazyparam_prover/tableau.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/ctx.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/enum_flag.h"
#include "lazyparam_prover/prover.pb.h"
#include "lazyparam_prover/lazy.h"
#include "solutions.pb.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/time.h"

const tableau::EnumFlag trans_def(prover::Transformation_descriptor());

ABSL_FLAG(absl::Duration,timeout,absl::Seconds(4),"spend timeout+eps time on searching");
ABSL_FLAG(tableau::EnumFlag,trans,trans_def,trans_def.values());
ABSL_FLAG(bool,trans_only,false,"");

using namespace tableau;

OrForm apply_trans(OrForm f) {
  // TODO: restrict it further to the case when equality atoms are reachable from all possible starting clause sets
  // TODO: look for starting sets with unreachable clauses - iteratively eliminate them
  if(!has_equality(f)) { return f; }
  auto trans = absl::GetFlag(FLAGS_trans);
  switch(trans.get()) {
  case prover::AXIOMATIC_EQ:
    return append_eq_axioms(f);
  case prover::AXIOMATIC_EQ_FLAT:
    return reduce_monotonicity_and_append_eq_axioms(f);
  case prover::AXIOMATIC_EQ_RESTRICTED_TRANS:
    return append_eq_axioms_with_restricted_transitivity(f);
  case prover::LP_MODIFICATION:
    return lazy::conv(f);
  default:
    error("trans = %",show(trans));
  }
}

Atom remove_eq(Atom a, u64 pred) {
  if(a.pred()!=Atom::EQ) return a;
  Atom::Builder b(a.sign(),pred,a.arg_count());
  for(size_t i=a.arg_count(); i--;) b.set_arg(i,a.arg(i));
  return b.build();
}

AndClause remove_eq(AndClause cla, u64 pred) {
  AndClause::Builder b(cla.atom_count());
  for(size_t i=cla.atom_count(); i--;) b.set_atom(i,remove_eq(cla.atom(i),pred));
  return b.build();
}

DerAndClause remove_eq(DerAndClause cla, u64 pred) {
  auto b = cla.to_builder();
  b.derived = remove_eq(b.derived,pred);
  for(auto &s : b.sources) s = remove_eq(s,pred);
  return b.build();
}

OrForm remove_eq(OrForm f) {
  ArityCtx actx; actx.traverse(f);
  u64 pred = 0;
  for(auto p : actx.pred_arity) util::maxi(pred,p.first);
  for(auto &cla : f.and_clauses) cla = remove_eq(cla,pred+1);
  return f;
}

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);

  str file_raw((std::istreambuf_iterator<char>(std::cin)), (std::istreambuf_iterator<char>()));
  ParseCtx parse_ctx;
  OrForm f(parse_ctx.parse_orForm(file_raw));

  auto emergency_block = new char[1000*1000];
  try {
    solutions::ProverOutput outProto;
    ProtoCtx pctx(parse_ctx);
    f = apply_trans(f);
    if(absl::GetFlag(FLAGS_trans_only)) {
      f = remove_eq(f);
      *outProto.mutable_transformed_problem() = pctx.proto_orForm(f);
      if(!outProto.SerializeToOstream(&std::cout)) {
        error("outProto.SerializeToOstream() failed");  
      }
      return 0;
    }

    auto ctx = Ctx::with_timeout(absl::GetFlag(FLAGS_timeout));
    auto out = prove_loop(*ctx,f);
    std::cerr << profile.show(); 
    
    outProto.set_cost(out.cost);
    outProto.set_continuation_count(out.cont_count);
    if(out.proof){
      outProto.set_solved(true);
      *outProto.mutable_proof() = pctx.proto_Proof(*out.proof,out.val);
    }
    if(!outProto.SerializeToOstream(&std::cout)) {
      error("outProto.SerializeToOstream() failed");  
    }
  } catch(const std::bad_alloc&) {
    delete []emergency_block;
    solutions::ProverOutput outProto;
    outProto.set_oom(true);
    outProto.SerializeToOstream(&std::cout);
  }
  return 0;
}
