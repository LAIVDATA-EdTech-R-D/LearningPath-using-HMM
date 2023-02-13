#-------------------------------------------------------------------------------------------------------------#
# program_name: plspm
# programe_descript: plspm 수행 및 평가값 출력
# file_path:~/바탕화면/YunjiLee/pls_sem
# 작성자: 이윤지
#-------------------------------------------------------------------------------------------------------------#
# plspm
install.packages("plspm")
install.packages("https://cran.r-project.org/src/contrib/Archive/plspm/plspm_0.4.9.tar.gz", repos = NULL, type="source")
library(plspm)
install.packages("devtools")
library(devtools)


#내부모형 행렬의 열
# 행: 받는것 열: 주는 것이라고 생각
# 하삼각행렬이어야함(대각선 0 아래에만 값이 있도록)
concept1=c(0,0,0,0,0,0)
concept2=c(1,0,0,0,0,0)
concept3=c(0,1,0,0,0,0)
concept4=c(0,0,1,0,0,0)
concept5=c(0,0,0,1,0,0)
concept6=c(0,0,0,0,1,0)
# 열 결합을 통한 경로행렬 벡터 생성
tam_path=rbind(concept1,concept2,concept3,concept4,concept5,concept6)
colnames(tam_path)=rownames(tam_path)
tam_blocks=list(1:2,3:5,6:9,10:13,14:16,17:22) #concept 각각에 해당하는 KC의 열(ex. concept1=열1~열2)
tam_blocks
tam_modes=c("A","A","A","A","A","A") #반영모델이니 B가 아닌 A
# 실행
tam_pls=plspm(level_13_clean, tam_path, tam_blocks, modes=tam_modes, boot.val=TRUE, br=500)

## 측정모형 평가
# 신뢰도
tam_pls$unidim #크론바흐 알파, dg.rho 등
# 지표신뢰도
tam_pls$boot$loadings # 표준화 적재량
# 판별타당도
tam_pls$crossloadings # 교차적재량
# 잠재변수들간의 상관계수 생성(AVE 제곱근 값이 해당 잠재변수와  다른 잠재변수들 간의 상관계수 값들보다 높아야함.)
tam_cor = cor(tam_scores, use = 'complete.obs',
                  +                 method = 'spearman') 
tam_pls$inner_summary$SQRT_AVE = with(tam_pls$inner_summary, sqrt(AVE))
cbind(tam_cor, tam_pls$inner_summary[-1:-5])

## 구조모형 평가
tam_pls$boot$paths$t_value = with(tam_pls$boot$paths, Original/Std.Error) #t-value
tam_pls$boot$paths$t_value
tam_pls$boot$rsq # R square
tam_pls$gof # goodness-of-fit
