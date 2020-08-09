#define DEBUG_MODE
//#define VERBOSE
//#define PROFILE

#include <csignal>
#include <iostream>
#include "lazyparam_prover/lazy_paramodulation/tableau.h"
#include "lazyparam_prover/connection_tableau/tableau.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/ctx.h"
#include "lazyparam_prover/log.h"
#include "lazyparam_prover/enum_flag.h"
#include "lazyparam_prover/prover.pb.h"
#include "lazyparam_prover/lpmod.h"
#include "solutions.pb.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/time.h"

const tableau::EnumFlag method_def(prover::Method_descriptor());
const tableau::EnumFlag trans_def(prover::Transformation_descriptor());
ABSL_FLAG(absl::Duration,timeout,absl::Seconds(4),"spend timeout+eps time on searching");
ABSL_FLAG(tableau::EnumFlag,method,method_def,method_def.values());
ABSL_FLAG(tableau::EnumFlag,trans,trans_def,trans_def.values());
ABSL_FLAG(bool,trans_only,false,"");

using namespace tableau;

OrForm apply_trans(memory::Alloc &A, OrForm f) {
  // TODO: restrict it further to the case when equality atoms are reachable from all possible starting clause sets
  // TODO: look for starting sets with unreachable clauses - iteratively eliminate them
  if(!has_equality(f)) { return f; }
  auto trans = absl::GetFlag(FLAGS_trans);
  switch(trans.get()) {
  case prover::SKIP:
    return f;
  case prover::AXIOMATIC_EQ:
    return append_eq_axioms(A,f);
  case prover::AXIOMATIC_EQ_FLAT:
    return reduce_monotonicity_and_append_eq_axioms(A,f);
  case prover::AXIOMATIC_EQ_RESTRICTED_TRANS:
    return append_eq_axioms_with_restricted_transitivity(A,f);
  case prover::LP_MODIFICATION:
    return lpmod::conv(A,f);
  default:
    error("trans = %",show(trans));
  }
}

template<typename Proto> inline static Proto proto_from_raw(const str &file_raw) { FRAME("proto_from_raw()");
  Proto proto;
  auto stream = new google::protobuf::io::CodedInputStream((const uint8_t*)(&file_raw[0]),file_raw.size());
  stream->SetRecursionLimit(100000000);
  if(!proto.ParseFromCodedStream(stream)) {
    error("failed to parse input");
  }
  return proto;
}

StreamLogger _(std::cerr);
int main(int argc, char **argv) {
  std::ios::sync_with_stdio(0);
  absl::ParseCommandLine(argc, argv);

  str input_raw((std::istreambuf_iterator<char>(std::cin)), (std::istreambuf_iterator<char>()));
  auto input = proto_from_raw<solutions::ProverInput>(input_raw);
  
  memory::Alloc A;
  OrForm f(ParseCtx().parse_orForm(A,input.problem()));
  RevNodeIndex idx(input.problem().nodes());

  auto emergency_block = new char[1000*1000];
  try {
    solutions::ProverOutput outProto;
    ProtoCtx pctx(idx);
    f = apply_trans(A,f);
    if(absl::GetFlag(FLAGS_trans_only)) {
      auto proto_f = pctx.proto_orForm(f);
      // replace equality with a regular predicate.
      for(auto &n : *proto_f.mutable_nodes()) {
        if(n.type()==tptp::PRED_EQ) {
          n.set_type(tptp::PRED);
          n.set_arity(2);
        }
      }
      *outProto.mutable_transformed_problem() = proto_f;
      if(!outProto.SerializeToOstream(&std::cout)) {
        error("outProto.SerializeToOstream() failed");  
      }
      return 0;
    }

    auto ctx = Ctx::with_timeout(absl::GetFlag(FLAGS_timeout));
    auto method = absl::GetFlag(FLAGS_method);
    ProverOutput out;
    FunOrd fun_ord(input.fun_ord(),input.problem().nodes());
    switch(method.get()) {
      case prover::CONNECTION_TABLEAU:
        out = connection_tableau::prove_loop(*ctx,A,f,fun_ord);
        break;
      case prover::LAZY_PARAMODULATION:
        if(absl::GetFlag(FLAGS_trans).get()!=prover::SKIP) {
          error("method=LAZY_PARAMODULATION requires trans=SKIP");
        }
        out = lazy_paramodulation::prove_loop(*ctx,A,f,fun_ord);
        break;
      default:
        error("method = %",show(method));
    }
    std::cerr << profile.show(); 
    
    outProto.set_cost(out.cost);
    outProto.set_continuation_count(out.cont_count);
    *outProto.mutable_stats() = out.stats.to_proto();
    if(out.proof){
      outProto.set_solved(true);
      *outProto.mutable_proof() = pctx.proto_Proof(A,*out.proof,out.val);
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
