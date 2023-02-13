#-------------------------------------------------------------------------------------------------------------#
# program_name: plspm_every_eval
# programe_descript: 모든 가능 경로의 plspm 수행 및 평가값 출력
# file_path:~/바탕화면/YunjiLee/pls_sem
# 작성자: 이윤지
#-------------------------------------------------------------------------------------------------------------#
a=c(0,0,0,0,0,0)
b=c(1,0,0,0,0,0)
c=c(0,1,0,0,0,0)
d=c(0,0,1,0,0,0)
e=c(0,0,0,1,0,0)
f=c(0,0,0,0,1,0)



mat<-rbind(a,b,c,d,e,f)
mat
str(mat)
mat # 6x6
num=c(1:6)

# 시작
path_df=data.frame(name=c()) # 리스트 비워주고~
tam_lst=list()
num=c(1:6)
tam_path=mat

for (i in num) { #남은 숫자들끼리
  for (j in num[c(-i)]) {
    for (k in num[c(-i, -j)]) {
      for (l in num [c(-i, -j, -k)]) {
        for (m in num [c(-i, -j, -k, -l)]) {
          for (n in num [c(-i, -j, -k, -l, -m)]){
          path_name=paste(i,j,k,l,m,n,collapse='-')
          path_df=rbind(path_df,path_name)
          colnames(tam_path)=c(paste0('concept',as.character(i)),paste0('concept',as.character(j)),
                                 paste0('concept',as.character(k)),paste0('concept',as.character(l)),paste0('concept',as.character(m)),paste0('concept',as.character(n)))
          rownames(tam_path)=colnames(tam_path)
          tam_lst<-c(tam_lst, list(path_name=tam_path))
            
          }
        }
      }
    }
  }
}
unlist(path_lst)

# pslpm 수행
tam_blocks=list('concept1'=1:2,'concept2'=3:5,'concept3'=6:9,'concept4'=10:13,'concept5'=14:16,'concept6'=17:22)
tam_modes=c("A","A","A","A","A","A")

eval_list<-list(path=c(),gof=c(),tvalue=c(),rsq=c())
for (i in 1:720){   # KC와 concept 매핑 시키기
  new_blocks=list()
  new_blocks=list(tam_blocks[[colnames(tam_lst[[i]])[1]]],tam_blocks[[colnames(tam_lst[[i]])[2]]],
                  tam_blocks[[colnames(tam_lst[[i]])[3]]],tam_blocks[[colnames(tam_lst[[i]])[4]]],
                  tam_blocks[[colnames(tam_lst[[i]])[5]]],tam_blocks[[colnames(tam_lst[[i]])[6]]])
  print(new_blocks)
  tam_pls=plspm(level_13_clean, tam_lst[[i]], new_blocks, modes=tam_modes,boot.val=TRUE, br=500)
  eval_list$path<-c(eval_list$path,paste0(colnames(tam_lst[[i]])))
  eval_list$gof<-c(eval_list$gof,tam_pls$gof)
  eval_list$tvalue<-c(eval_list$tvalue,with(tam_pls$boot$paths, Original/Std.Error))
  eval_list$rsq<-c(eval_list$rsq,tam_pls$boot$rsq)
}

df_path<-as.data.frame(eval_list)

gof_list$path<-NULL

df_gof<-as.data.frame(eval_list$gof)
df_tvalue<-as.data.frame(eval_list$tvalue)
df_rsq<-as.data.frame(eval_list$rsq)
result<-cbind(path_df,df_gof,df_tvalue,df_rsq)

max(result$gof)
names(result)<-c("path_name","gof","t_value","rsq")

View(result)
