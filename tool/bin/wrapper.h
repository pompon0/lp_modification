#ifndef TOOL_BIN_WRAPPER_H_
#define TOOL_BIN_WRAPPER_H_

#include "tool/bin/bin.pb.h"
#include "utils/log.h"
#include "utils/sys.h"
#include "utils/bazel.h"

namespace tool::bin {

static ::tool::Response wrapper(Ctx::Ptr ctx, ::tool::Request req) {
  const str bin_path = "__main__/tool/bin/bin_/bin";
  str reqstr;
  if(!req.SerializeToString(&reqstr)) {
    error("req.SerializeToString()");
  }
  str respstr = utils::sys::subprocess(ctx,{util::runfile(bin_path)},reqstr);
  ::tool::Response resp;
  if(!resp.ParseFromString(respstr)) {
    error("resp.ParseFromString()");
  }
  return resp;
}

}

#endif  // TOOL_BIN_WRAPPER_H_
