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
meta = pd.read_csv('./data/aihub5.csv')

data.dropna(axis=0, inplace=True)
data.reset_index(drop=True, inplace=True)

permutation_list = list(itertools.permutations(list(data.columns)[1:], 2))

cols = list(data.columns)[1:]

for col in cols:
    data.loc[data[col] >= 0.6, col] = int(1)
    data.loc[data[col] < 0.6, col] = int(0)

states = ["unlearned", "learned"]
n_states = len(states)

observations = ["correct", "incorrect"]
n_observations = len(observations)

result_df = pd.DataFrame(columns=(['before', 'after', 'init_prob','V1', 'V2', 'V3','V4', 'before_name', 'after_name']))

for target1, target2 in permutation_list:
  add_ary = ["X"+str(target1), "X"+str(target2)]

  X = np.column_stack([data[target1].astype('int'), data[target2].astype('int')])
  start_probability = np.array([1-X.T[0].sum()/len(X), X.T[0].sum()/len(X)])

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
  #add_ary.append(model.emissionprob_)
  #add_ary.append("-")
  
  
  cond = meta['knowledgeTag'] == int(target1)
  chapter_name1 = meta[cond]['chapter_name'].unique()[0]
  cond = meta['knowledgeTag'] == int(target2)
  chapter_name2 = meta[cond]['chapter_name'].unique()[0]

  add_ary.append(chapter_name1)
  add_ary.append(chapter_name2)
  
  result_df = result_df.append(pd.Series(add_ary, index=result_df.columns), ignore_index=True)

result_df = result_df.astype('str')
result_df.to_csv('./out/OnlyHMM_G_221227.csv', index=False, encoding='utf-8-sig')
