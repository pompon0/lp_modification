#ifndef FFPROVER_XGBOOST_H_
#define FFPROVER_XGBOOST_H_

#include "utils/types.h"
#include "utils/log.h"

#include "xgboost/data.h"
#include "xgboost/learner.h"
#include "xgboost/c_api.h"

namespace ff {

struct FeatureVec {
  enum { max_fea = 100 }; // bound on indices values
  void push(unsigned index, float datum) {
    indices.push_back(index);
    data.push_back(datum);
  } 
private:
  friend struct Matrix;
  vec<unsigned> indices;
  vec<float> data;
};

struct Matrix {
  static ptr<Matrix> New(const FeatureVec &f) {
    DMatrixHandle handle;
    size_t indptr[] = {0,f.indices.size()};
    if(auto err = XGDMatrixCreateFromCSREx(indptr, &f.indices[0], &f.data[0], 2, f.indices.size(), f.max_fea, &handle); err!=0) {
      error("XGDMatrixCreateFromCSREx() = %",err);
    }
    return own(new Matrix(handle));
  }

  ~Matrix() {
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
  static ptr<Model> New(const str &model_path) {
    if(model_path=="") {
      return own(new Model());
    }
    BoosterHandle handle;
    if(auto err = XGBoosterCreate(0 /*dmat*/, 0 /*len(dmat)*/, &handle); err!=0) {
      error("XGBoosterCreate(): %",XGBGetLastError());
    }
    if(auto err = XGBoosterLoadModel(&handle, model_path.c_str()); err!=0) {
      error("XGBoosterLoadModel(): %",XGBGetLastError());
    }
    return own(new Model(handle));
  }
  ~Model() {
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

/*value x_mem(value unit) {
  CAMLparam1 (unit);
  struct rusage r_usage;
  getrusage(RUSAGE_SELF,&r_usage);
  CAMLreturn (Val_int(r_usage.ru_maxrss));
}*/

}  // namespace ff

#endif  // FFPROVER_XGBOOST_H_
