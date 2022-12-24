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

result_df = pd.DataFrame(columns=(['before', 'after', 'init_prob','trans_prob','emiss_prob', 'before_name', 'after_name']))

for target1, target2 in permutation_list:
  add_ary = [target1, target2]

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

  model = hmm.CategoricalHMM(n_components=n_states, n_iter=len(X), startprob_prior=start_probability, 
                                transmat_prior=transition_probability, emissionprob_prior=emission_probability, params = 's')

  model.fit(X)

  add_ary.append(model.startprob_)
  add_ary.append(model.transmat_)
  add_ary.append(model.emissionprob_)
  
  cond = meta['knowledgeTag'] == int(target1)
  chapter_name1 = meta[cond]['chapter_name'].unique()[0]
  cond = meta['knowledgeTag'] == int(target2)
  chapter_name2 = meta[cond]['chapter_name'].unique()[0]

  add_ary.append(chapter_name1)
  add_ary.append(chapter_name2)
  
  result_df = result_df.append(pd.Series(add_ary, index=result_df.columns), ignore_index=True)
result_df.to_csv('./out/OnlyHMM_test_221223.csv', index=False, encoding='utf-8-sig')
