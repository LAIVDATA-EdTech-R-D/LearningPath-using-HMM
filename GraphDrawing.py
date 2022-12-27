import pandas as pd
from graphviz import Digraph

# AIHub target data
target = pd.read_csv('./out/cal_result_alph3_FSHMM.csv')

kcs = ['X3751', 'X3752' ,'X3753' ,'X3755' ,'X3756', 'X3757', 'X3824' ,'X3825' ,'X3826' ,'X3827', 'X3828' ,'X5258', 'X5259']
keyss = ["KC01","KC02","KC03","KC04","KC05","KC06","KC07","KC08","KC09","KC10","KC11","KC12","KC13"]
rename_dic = dict(zip(kcs, keyss))
alls = list(set(list(target['before'].unique()) + list(target['after'].unique())))

limitKC = pd.DataFrame(columns=target.columns)

for i in range(len(target)):
    if (target['before'][i] in kcs) and (target['after'][i] in kcs):
        limitKC = pd.concat([limitKC, pd.DataFrame(target.loc[i][target.columns]).T])
limitKC = limitKC.drop_duplicates()
limitKC.reset_index(drop=True, inplace=True)
# limitKC = target.copy()

## Directed Graph
g = Digraph(name='Directed', ## 그래프 이름
          filename='FSHMM_Graph_all_221227', ## 파일 이름
          format='png', ## 파일 형식
          directory='./out/', ## 파일 저장 디렉토리
          )

# node 생성
for n in keyss:
    g.node(name=n)

"""for n in alls:
    g.node(name=n)
"""

# edge
for i in range(len(limitKC)):
    if limitKC['pred'][i] == 0:
        if limitKC['target'][i] == 1:
            # 회색 점선
            before = limitKC['before'][i]
            after = limitKC['after'][i]
            g.edge(rename_dic[before], rename_dic[after], color='gray', style='dashed')
            #g.edge(before, after, color='gray', style='dashed')
        else:
            print("something wrong")
    else:
        if limitKC['target'][i] == 1:
            # 파란 실선
            before = limitKC['before'][i]
            after = limitKC['after'][i]
            g.edge(rename_dic[before], rename_dic[after], color='blue')
            #g.edge(before, after, color='blue')
        else:
            # 빨강 실선
            before = limitKC['before'][i]
            after = limitKC['after'][i]
            g.edge(rename_dic[before], rename_dic[after], color='red')
            #g.edge(before, after, color='red')

# save
g.render(filename='FSHMM_Graph_all_221227', directory='./out/')
