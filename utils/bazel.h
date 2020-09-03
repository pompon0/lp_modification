#ifndef UTILS_BAZEL_H_
#define UTILS_BAZEL_H_

#include <cstdlib>
#include <filesystem>
#include "utils/log.h"

namespace util {

static str runfiles_root() {
  if(auto dir = std::getenv("RUNFILES_DIR"); dir!=0) { return dir; }
  std::error_code err;
  auto wd = std::filesystem::current_path(err);
  if(err) error("current_path(): %",err.message());
  return wd.parent_path(); 
}

static void show_runfiles() {
  for(auto entry : std::filesystem::recursive_directory_iterator(runfiles_root())) {
    info("'%'",str(entry.path()));
  }
}

static std::filesystem::path runfile(std::filesystem::path workspace_path) {
  auto path = runfiles_root() / workspace_path;
  if(!std::filesystem::exists(path)) {
    show_runfiles();
    error("runfile '%' doesn't exist",str(path));
  }
  return path;
}

}  // utils

#endif  // UTILS_BAZEL_H_
