#!/usr/bin/python
import math
import numpy as np
import scipy.sparse.csr
import sklearn.datasets
import sys
import xgboost as xgb

n_features = 2**15

x_t, y_t = sklearn.datasets.load_svmlight_file(sys.argv[1], n_features=n_features, zero_based=1)
d_t = xgb.DMatrix(x_t, label=y_t)
#d_t = xgb.DMatrix(sys.argv[1])
#y_t = d_t.get_label()
#if len(sys.argv) > 1:
#    d_v = xgb.DMatrix(sys.argv[2])

params = {}
params['objective'] = 'reg:squarederror'
params['eval_metric'] = ['mae','rmse']
params['eta'] = .3
params['max_depth'] = 9
params['nthread'] = 4
params['lambda'] = 1.5

#if len(sys.argv) > 1:
#    watchlist = [(d_t, 'tr'), (d_v, 'val')]
#else:
watchlist = [(d_t, 'tr')]

rf = xgb.train(params, d_t, 50, watchlist, early_stopping_rounds=50, verbose_eval=10)
# print rf.predict(d_t)

mae=np.sum(np.abs(rf.predict(d_t) - y_t)) / len(y_t)
rmse=math.sqrt(np.sum(np.square(rf.predict(d_t) - y_t)) / len(y_t))
# print "trained %s mae: %.4f rmse: %.4f" % (sys.argv[1], mae, rmse)
avg = sum(y_t) / len(y_t)
mae=np.sum(np.abs(avg - y_t)) / len(y_t)
rmse=math.sqrt(np.sum(np.square(avg - y_t)) / len(y_t))
# print "avg %f mae: %.4f rmse: %.4f" % (avg, mae, rmse)

rf.save_model(sys.argv[2])

