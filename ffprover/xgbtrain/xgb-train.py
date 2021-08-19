#!/usr/bin/python
import math
import numpy as np
import scipy.sparse.csr
import sklearn.datasets
import sys
import xgboost as xgb
import argparse
import logging

parser = argparse.ArgumentParser()
parser.add_argument('--features_space_size', type=int, required=True)
args = parser.parse_args()

x_t, y_t = sklearn.datasets.load_svmlight_file(sys.stdin.buffer, n_features=args.features_space_size, zero_based=1)
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

# TODO: with verbose_eval, this shit writes to stdout (and it is not configurable).
# To be resistant to this dumb behavior, we should:
# - duplicate descriptor 1 -> new one
# - duplicate descriptor 2 -> 1
rf = xgb.train(params, d_t, 50, watchlist, early_stopping_rounds=50, verbose_eval=False)
# print rf.predict(d_t)

mae=np.sum(np.abs(rf.predict(d_t) - y_t)) / len(y_t)
rmse=math.sqrt(np.sum(np.square(rf.predict(d_t) - y_t)) / len(y_t))
# print "trained %s mae: %.4f rmse: %.4f" % (sys.argv[1], mae, rmse)
avg = sum(y_t) / len(y_t)
mae=np.sum(np.abs(avg - y_t)) / len(y_t)
rmse=math.sqrt(np.sum(np.square(avg - y_t)) / len(y_t))
# print "avg %f mae: %.4f rmse: %.4f" % (avg, mae, rmse)

sys.stdout.buffer.write(rf.save_raw())
sys.stdout.buffer.flush()
