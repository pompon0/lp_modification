#ifndef FFPROVER_XGBOOST_H_
#define FFPROVER_XGBOOST_H_

#include "utils/types.h"
#include "utils/log.h"

#include "xgboost/data.h"
#include "xgboost/learner.h"
#include "xgboost/c_api.h"

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
  };
  
  vec<Instance> instances;
  str show() const {
    vec<str> is;
    for(const auto &i : instances) is.push_back(i.show()+"\n");
    return util::join("",is);
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
  static ptr<Model> New(const str &model_path) { FRAME("Model::New(%)",model_path);
    if(model_path=="") {
      return own(new Model());
    }
    BoosterHandle handle;
    info("XGBoosterCreate()");
    if(auto err = XGBoosterCreate(0 /*dmat*/, 0 /*len(dmat)*/, &handle); err!=0) {
      error("XGBoosterCreate(): %",XGBGetLastError());
    }
    info("XGBoosterLoadModel()");
    // WARNING: this compiles without error, no matter if you put handle (void*),
    // or &handle (void**), which sucks.
    if(auto err = XGBoosterLoadModel(handle, model_path.c_str()); err!=0) {
      error("XGBoosterLoadModel(): %",XGBGetLastError());
    }
    info("Done");
    return own(new Model(handle));
  }
  ~Model() { FRAME("~Model()");
    if(initialized) {
      if(auto err = XGBoosterFree(handle_); err!=0) {
        error("XGBoosterFree(): %",XGBGetLastError());
      }
    }
  }

  float predict(const FeatureVec &f) {
    if(!initialized) return 1.;
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
  Model() : initialized(false), handle_{} {}
  Model(BoosterHandle _handle) : initialized(true), handle_(_handle) {}
  bool initialized;
  BoosterHandle handle_; 
};

}  // namespace ff

#endif  // FFPROVER_XGBOOST_H_
