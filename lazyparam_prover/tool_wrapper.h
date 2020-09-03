#ifndef TOOL_WRAPPER_H_
#define TOOL_WRAPPER_H_

#include "utils/ctx.h"
#include "utils/sys.h"
#include "utils/bazel.h"
#include "tptp.pb.h"

template<typename Proto> inline static Proto proto_from_raw(const str &file_raw) { FRAME("proto_from_raw()");
  Proto proto;
  auto stream = new google::protobuf::io::CodedInputStream((const uint8_t*)(&file_raw[0]),file_raw.size());
  stream->SetRecursionLimit(100000000);
  if(!proto.ParseFromCodedStream(stream)) {
    error("failed to parse input");
  }
  return proto;
}

static tptp::File tptp_to_proto(Ctx::Ptr ctx, const str &tptp) {
  const str cc_tool_bin_path = "__main__/lazyparam_prover/tool";
  auto proto_raw = sys::subprocess(ctx,{util::runfile(cc_tool_bin_path)},tptp);
  return proto_from_raw<tptp::ToolOutput>(proto_raw).file();
}

#endif  // TOOL_WRAPPER_H_
