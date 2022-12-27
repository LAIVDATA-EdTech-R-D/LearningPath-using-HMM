import numpy as np
import pandas as pd
import itertools
import random
from hmmlearn import hmm

import warnings
warnings.filterwarnings('ignore')

random.seed(42)
np.random.seed(42)

data = pd.read_csv('./data/aihub5_reshape.csv')
meta = pd.read_csv('./out/rf_elasso_221227.csv')

data.dropna(axis=0, inplace=True)
data.reset_index(drop=True, inplace=True)

cols = list(data.columns)[1:]
# binary로 바꾸기
for col in cols:
    data.loc[data[col] >= 0.6, col] = int(1)
    data.loc[data[col] < 0.6, col] = int(0)
#data.to_csv('./data/aihub5_binary_nonNa.csv', index=False)


states = ["unlearned", "learned"]
n_states = len(states)

result_df = pd.DataFrame(columns=(['before', 'after', 'imp', 'method', 'init_prob','V1', 'V2', 'V3','V4', 'before_name', 'after_name']))

for i in range(len(meta)):
    target1 = meta['before'][i][2:]
    target2 = meta['after'][i][2:]
    
    add_ary = ["X"+str(target1), "X"+str(target2)]
    add_ary.append(meta['imp'][i])
    add_ary.append(meta['method'][i])

    X = np.column_stack([data[target1].astype('int'), data[target2].astype('int')])
    start_probability = np.array([1 - X.T[0].sum()/len(X), X.T[0].sum()/len(X)])

    # z zero / o one
    zz, zo, oz, oo = 0, 0, 0, 0
    for d in X:
        if d[0] == 0:
            if d[1] == 0: zz += 1
            else: zo += 1
        else:
            if d[1] == 0: oz += 1
            else: oo += 1

    transition_probability = np.array([
        [zz/(zz+zo), zo/(zz+zo)],
        [oz/(oz+oo), oo/(oz+oo)]
    ])

    emission_probability = np.array([
        [0.9, 0.1],
        [0.1, 0.9]
    ])

    model = hmm.GaussianHMM(n_components=n_states, n_iter=500, params = 's')
    model.transmat_ = transition_probability
    model.init_params = start_probability

    model.fit(X)

    add_ary.append(model.startprob_)
    add_ary.append(model.transmat_[0][0]) # V1
    add_ary.append(model.transmat_[1][0]) # V2
    add_ary.append(model.transmat_[0][1]) # V3
    add_ary.append(model.transmat_[1][1]) # V4

    add_ary.append(meta['before_name'][i])
    add_ary.append(meta['after_name'][i])
    
    result_df = result_df.append(pd.Series(add_ary, index=result_df.columns), ignore_index=True)



    target1 = meta['after'][i][2:]
    target2 = meta['before'][i][2:]
    add_ary = ["X"+str(target1), "X"+str(target2)]
    add_ary.append(meta['imp'][i])
    add_ary.append(meta['method'][i])

    X = np.column_stack([data[target1].astype('int'), data[target2].astype('int')])
    start_probability = np.array([1 - X.T[0].sum()/len(X), X.T[0].sum()/len(X)])

    # z zero / o one
    zz, zo, oz, oo = 0, 0, 0, 0
    for d in X:
        if d[0] == 0:
            if d[1] == 0: zz += 1
            else: zo += 1
        else:
            if d[1] == 0: oz += 1
            else: oo += 1

    transition_probability = np.array([
        [zz/(zz+zo), zo/(zz+zo)],
        [oz/(oz+oo), oo/(oz+oo)]
    ])

    emission_probability = np.array([
        [0.9, 0.1],
        [0.1, 0.9]
    ])

    model = hmm.GaussianHMM(n_components=n_states, n_iter=500, params = 's')
    model.transmat_ = transition_probability
    model.init_params = start_probability

    model.fit(X)

    add_ary.append(model.startprob_)
    add_ary.append(model.transmat_[0][0]) # V1
    add_ary.append(model.transmat_[1][0]) # V2
    add_ary.append(model.transmat_[0][1]) # V3
    add_ary.append(model.transmat_[1][1]) # V4

    add_ary.append(meta['after_name'][i])
    add_ary.append(meta['before_name'][i])
    
    result_df = result_df.append(pd.Series(add_ary, index=result_df.columns), ignore_index=True)

result_df = result_df.astype('str')
result_df.to_csv('./out/FSHMM_G_221227.csv', index=False, encoding='utf-8-sig')
