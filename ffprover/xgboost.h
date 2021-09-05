#ifndef FFPROVER_XGBOOST_H_
#define FFPROVER_XGBOOST_H_

#include "utils/types.h"
#include "utils/log.h"

#include "xgboost/data.h"
#include "xgboost/learner.h"
#include "xgboost/c_api.h"

#include "ffprover/mcts.pb.h"

namespace ff {

struct FeatureVec {
  FeatureVec(size_t _space_size) : space_size(_space_size) {}
  void push(unsigned index, float value) {
    if(index>=space_size) error("index % > space_size = %",index,space_size);
    indices.push_back(index);
    values.push_back(value);
  }
  str show() const {
    vec<str> fs;
    for(size_t i=0; i<indices.size(); i++)
      if(values[i]!=0.) fs.push_back(util::fmt("%:%",indices[i],std::to_string(values[i])));
    return util::join(" ",fs);
  }

  void to_proto(mcts::LibSVM::Instance &inst) const {
    for(size_t i=0; i<indices.size(); i++) if(values[i]!=0.) {
      auto f = inst.add_features();
      f->set_index(indices[i]);
      f->set_value(values[i]);
    }
  }
private:
  friend struct Matrix;
  size_t space_size;
  vec<unsigned> indices;
  vec<float> values;
};

// TODO: dataset show() is xgboost-specific,
// so it doesn't belong here. Move is somewhere else.
struct DataSet {
  struct Instance {
    double label;
    FeatureVec features;
    str show() const { return util::fmt("% %",label,features.show()); }
    void to_proto(mcts::LibSVM::Instance &inst) const {
      features.to_proto(inst);
      inst.set_label(label);
    }
  };
  
  vec<Instance> instances;
  str show() const {
    vec<str> is;
    for(const auto &i : instances) is.push_back(i.show()+"\n");
    return util::join("",is);
  }
  void to_proto(mcts::LibSVM &data) const {
    for(auto &i : instances) i.to_proto(*data.add_instances());
  }
};

struct Matrix {
  static ptr<Matrix> New(const FeatureVec &f) { FRAME("Matrix::New()");
    DMatrixHandle handle;
    size_t indptr[] = {0,f.indices.size()};
    if(auto err = XGDMatrixCreateFromCSREx(indptr, &f.indices[0], &f.values[0], 2, f.indices.size(), f.space_size, &handle); err!=0) {
      error("XGDMatrixCreateFromCSREx() = %",err);
    }
    return own(new Matrix(handle));
  }

  ~Matrix() { FRAME("~Matrix()");
    if(auto err = XGDMatrixFree(handle_); err!=0) {
      error("XGDMatrixFree(): %",XGBGetLastError());
    } 
  }
  DMatrixHandle handle(){ return handle_; }
private:
  Matrix(DMatrixHandle _handle) : handle_(_handle) {}
  DMatrixHandle handle_;
};

struct Model {
  static ptr<Model> New(const mcts::Model &proto) { FRAME("Model::New()");
    BoosterHandle handle;
    info("XGBoosterCreate()");
    if(auto err = XGBoosterCreate(0 /*dmat*/, 0 /*len(dmat)*/, &handle); err!=0) {
      error("XGBoosterCreate(): %",XGBGetLastError());
    }
    info("XGBoosterLoadModel()");
    // WARNING: this compiles without error, no matter if you put handle (void*),
    // or &handle (void**), which sucks.
    if(auto err = XGBoosterLoadModelFromBuffer(handle, proto.xgb().c_str(), proto.xgb().size()); err!=0) {
      error("XGBoosterLoadModelFromBuffer(): %",XGBGetLastError());
    }
    info("Done");
    return own(new Model(handle));
  }
  ~Model() { FRAME("~Model()");
    if(auto err = XGBoosterFree(handle_); err!=0) {
      error("XGBoosterFree(): %",XGBGetLastError());
    }
  }

  float predict(const FeatureVec &f) {
    auto mtx = Matrix::New(f); 
    const int training = true;
    bst_ulong res_len;
    const float *res;
    if(auto err = XGBoosterPredict(handle_, mtx->handle(), 0, 0, training, &res_len, &res); err!=0) {
      error("XGBoosterPredict(): %",XGBGetLastError());
    }
    if(res_len!=1) error("res_len = %",res_len);
    return *res;
  }
private:
  Model(BoosterHandle _handle) : handle_(_handle) {}
  BoosterHandle handle_; 
};

}  // namespace ff

#endif  // FFPROVER_XGBOOST_H_
