#ifndef PROBLEMS_SAMPLE_H_
#define PROBLEMS_SAMPLE_H_

#include <map>
#include <google/protobuf/text_format.h>
#include "utils/log.h"
#include "utils/bazel.h"
#include "utils/read_file.h"
#include "problems/sample/sample.pb.h"

namespace problems::sample {

static std::map<str,str> sample_problems() {
  const str sample_problems_path = "__main__/problems/sample/sample.textpb"; 
  auto raw = util::read_file(util::runfile(sample_problems_path));
  str rawstr(raw.begin(),raw.end());

  problems::Set problemSet;
  if(!google::protobuf::TextFormat::ParseFromString(rawstr,&problemSet)) {
    error("google::protobuf::TextFormat::ParseFromString() failed");
  }
  std::map<str,str> problems;
  for(auto p : problemSet.problems()) {
    problems[p.name()] = p.tptp();
  }
  return problems;
}

}

#endif  // PROBLEMS_SAMPLE_H_
