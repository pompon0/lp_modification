#ifndef UTILS_ENUM_FLAG_H_
#define UTILS_ENUM_FLAG_H_

#include "google/protobuf/descriptor.h"
#include "absl/strings/string_view.h"
#include "utils/string.h"

namespace tableau {

struct EnumFlag {
  explicit EnumFlag(const google::protobuf::EnumDescriptor *_desc) : desc(_desc), value(0) {}

  str values() const {
    vec<str> names;
    for(int i=0; i<desc->value_count(); i++) {
      names.push_back(desc->value(i)->name());
    }
    return util::join("|",names);
  }

  friend bool AbslParseFlag(absl::string_view text, EnumFlag *flag, std::string *error) {
    if(auto val = flag->desc->FindValueByName(std::string(text))) {
      flag->value = val->number(); 
      return true;
    }
    *error = flag->values();
    return false;
  }

  friend std::string AbslUnparseFlag(EnumFlag flag) { return show(flag); }

  friend str show(EnumFlag flag) {
    if(auto val = flag.desc->FindValueByNumber(flag.value)) {
      return val->name();
    }
    return "";
  }
  
  int get(){ return value; }
private:
  const google::protobuf::EnumDescriptor *desc;
  int value;
};

}  // namespace tableau

#endif  // UTILS_ENUM_FLAG_H_
