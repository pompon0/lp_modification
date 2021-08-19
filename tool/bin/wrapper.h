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

static str tptp_cnf(const str &tptp_fof) {
  ::tool::Request req;
  req.mutable_fof_to_cnf()->set_tptp_fof(tptp_fof);
  return wrapper(Ctx::background(),req).fof_to_cnf().tptp_cnf();
}

static tptp::File tptp_to_proto(const str &tptp_cnf) {
  ::tool::Request req;
  req.mutable_tptp_to_proto()->set_tptp(tptp_cnf);
  req.mutable_tptp_to_proto()->set_lang(tptp::Input::CNF);
  return wrapper(Ctx::background(),req).tptp_to_proto().file();
}

}

#endif  // TOOL_BIN_WRAPPER_H_
