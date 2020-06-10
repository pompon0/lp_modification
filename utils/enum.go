package utils

import (
  "strings"
  "fmt"
  "flag"
  "log"

  "google.golang.org/protobuf/reflect/protoreflect"
)

func NewEnumFlag(name string, def protoreflect.Enum) *protoreflect.EnumNumber {
  f := &EnumFlag{desc:def.Descriptor(),Value:def.Number()}
  flag.Var(f,name,f.Desc())
  return &f.Value
}

type EnumFlag struct {
  desc protoreflect.EnumDescriptor
  Value protoreflect.EnumNumber
}

func (f *EnumFlag) Desc() string {
  var names []string
  for i,n := 0,f.desc.Values().Len(); i<n; i++ {
    names = append(names,string(f.desc.Values().Get(i).Name()))
  }
  return strings.Join(names,"|")
}

func (f *EnumFlag) Set(name string) error {
  v := f.desc.Values().ByName(protoreflect.Name(name))
  if v==nil { return fmt.Errorf("unknown %q",name) }
  f.Value = v.Number()
  return nil
}

func (f *EnumFlag) String() string {
  // For some reason we need to support execution on nil
  // (https://golang.org/pkg/flag/#Value)
  if f.desc==nil {
    return ""
  }
  if v:=f.desc.Values().ByNumber(f.Value); v!=nil {
    return string(v.Name())
  }
  log.Fatalf("wtf")
  return ""
}
