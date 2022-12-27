##################################################################################################
# The purpose of this program is finding prerequisite relationship of 2KCs w/ elasso+rf
##################################################################################################

library(rjson)
library(aws.s3)
library(tictoc)
library(sqldf)
library(reshape2)
library(glmnet)
library(seqHMM)
library(TraMineR)
library(stringr)
library(purrr)
library(plyr)
library(caret)
library(randomForest)
library(plyr)

init <- function() {
  info <- fromJSON(file = "./PGM/info_aihub.json")
  
  # host parameters
  useaws <<- info[[1]]$useaws
  bucketname <<- info[[1]]$bucketname
  localpath <<- info[[1]]$localpath

  # csv file name
  lasso_p_value <<-info[[1]]$lasso_p_value
  rf_cut <<-info[[1]]$rf_cut
  grade <<- info[[1]]$grade
  print (useaws)
  print (useaws)
  print (bucketname)
  print (localpath)
  print (lasso_p_value)
  print (rf_cut)
  print (grade)
}  




#-------------------------------------------------------------------------------------------------------------#
#1. make a wide table obs : knowre_user_id, columns : kc_uid
#-------------------------------------------------------------------------------------------------------------#

make_reshaped_table  <- function(dat){
  dat1=as.data.frame(acast(dat,learnerID ~ knowledgeTag,value.var="answerCode", fun=mean))
 return(dat1)
}
#-------------------------------------------------------------------------------------------------------------#
#13. Since there is no order, birelation is not meaningful
#-------------------------------------------------------------------------------------------------------------#

clean_birel <- function(dat){

  dat2 = sqldf("select min(before, after) as before, max(before,after) as after,max(imp) as imp from dat group by before, after order by before, after")

  return(dat2)
}

#-------------------------------------------------------------------------------------------------------------#
#1. elastic net
#-------------------------------------------------------------------------------------------------------------#

elastic_anal <- function(dat){
  
  kcs=colnames(dat)
  
  lasso_relation <- data.frame(kc1=character(), kc2=character())  #Define a table to save relations b/w KC
  
  for(i in 1:length(kcs)){
    target_name = kcs[i]
    `%ni%`<-Negate("%in%")
    x<-model.matrix(dat[,target_name]~.,data=dat)
    x=subset(x, select= -c(which(startsWith(colnames(x), target_name))))
    set.seed(100) 
    glmnet1<-cv.glmnet(x=x,y=dat[,target_name],type.measure='mse',nfolds=as.integer(nrow(dat)/3) ,alpha=1)
    c<-coef(glmnet1,s='lambda.min',exact=TRUE)
    inds<-which(c!=0)
    variables<-row.names(c)[inds]
    variables<-variables[variables %ni% c('(Intercept)')]
    if(length(variables)==0) next
    dat_temp = dat[,c( target_name,variables)]
    
    formula1 = paste(target_name ,"~.")
    
    print(formula1)
    #   print(summary(lm(formula=formula1, dat_temp)))
    
    prob = as.matrix(summary(lm(formula=formula1, dat_temp))$coefficients)
    
    prob1 = row.names(prob[prob[,4]<= lasso_p_value,])  
    prob2 = prob1[prob1 %ni% '(Intercept)']
    if(length(prob2)==0) next
    
    for( j in 1:length(prob2)) lasso_relation = rbind(lasso_relation, cbind(prob2[j],target_name))
    
  }
  colnames(lasso_relation)=c("before","after")
  
  rel = sqldf("select before , after from lasso_relation  order by before, after")
  
  return(rel)
}  

#-------------------------------------------------------------------------------------------------------------#
#2. Run Random Forest
#-------------------------------------------------------------------------------------------------------------#

run_rf <- function(dat){
  
  imp_mat <- data.frame(before=character(), after=integer(),character(), imp = double())
  
  for(i in 1:ncol(dat)){
    print(paste0("Calculate importance values on ",colnames(dat)[i]))
    set.seed=500
    imp <-  importance(randomForest(dat[,-i], dat[,i],ntree=500, importance=T, scale=TRUE),type=1)
    imp_temp <- as.data.frame(cbind(rownames(imp), matrix(rep(colnames(dat)[i],nrow(imp)), nrow= nrow(imp), ncol=1,byrow=TRUE), imp[,1]))
    colnames(imp_temp) = c("before","after","imp")
    imp_mat = rbind(imp_mat,imp_temp)
  }
  imp_mat$imp=as.integer(imp_mat$imp)  
  print("Congratulations : Importance matrix was made !!!")
  return(imp_mat)
}


data_cleanse<- function(data){
  data1 <- data[complete.cases(data) ,] 
  data_clean <- data[rowSums(data1)> 0 & rowSums(data1)<ncol(data), colSums(data[]) !=nrow(data[]) & colSums(data[])!=0] # delete if low count
  print(paste0("original: ","(", ncol(data_clean),",",nrow(data),")", "cleansed: ","(", ncol(data_clean),",",nrow(data_clean),")"))
  return(data_clean)
}

#-------------------------------------------------------------------------------------------------------------#
#9 remove a variable randomly with a high correlation
#-------------------------------------------------------------------------------------------------------------#

drop_high_corr<-function(input_data){
  drop_KC_name=vector() # 빈 벡터
  droped_data=data.frame() # 빈 데이터프레임
  kc_seq=ncol(input_data) # kc 개수(n개)
  kc_seq_vec<-c(1:kc_seq) # kc 인덱스 벡터(1,2,..,n)
  for (i in 1:kc_seq){
    kc_seq_vec <- kc_seq_vec[! kc_seq_vec %in% i] # kc_seq_vec에서 i 인덱스를 제외
    for (j in kc_seq_vec){
      cor<-cor(input_data[i],input_data[j],method="spearman")
      if(cor>=0.9){
        choice<-c(input_data[i],input_data[j])
        chosen<-sample(choice,1)
        drop_KC_name=c(drop_KC_name,names(chosen))
        print(paste0('drop:',names(chosen)))
      }
    }
  }
  if (length(drop_KC_name)>=1){
    dropped_data<-input_data[names(input_data)!=drop_KC_name]
    print(paste0('before=',ncol(input_data)))
    print(paste0('after =',ncol(dropped_data)))
    return(dropped_data)
  }
  else {
    print('제거 X')
    return(input_data)
  }
}

read_aihub <- function(){
library(sqldf)

aihub5 = read.csv("./DATA/(merged)5th_all_utf_8_sig.csv") 

temp51 = sqldf("select knowledgeTag, chapter_id, chapter_name, count(distinct learnerID)  as no_std from aihub5 group by knowledgeTag")
temp52 = sqldf("select rank() over (order by no_std desc) as row_rank, chapter_name, chapter_id, knowledgeTag, no_std from temp51 where chapter_id is not null order by chapter_id")
write.csv(temp52,"aihub5_temp.csv" )

library(reshape2)

aihub_sel = sqldf("select * from aihub5 where knowledgeTag in (select knowledgeTag from temp52) and (knowledgeTag >3750 and knowledgeTag <5260) ")

dat1=as.data.frame(acast(aihub_sel,learnerID~ knowledgeTag, value.var="answerCode", fun=mean))
aihub.complete <- dat1[complete.cases(dat1) ,] 
nrow(aihub.complete)
print(" rows are complete")
colSums(is.na(dat1))


return(aihub.complete)
}

add_title <- function(df_chapter1,rf_elasso1){
#  df_chapter1 = df_chapter
#  rf_elasso1 = rf_elasso
  df_chapter1$knowledgeTag1 = paste0("KC",df_chapter1$knowledgeTag)

  temp1 = sqldf("select a.*, b.chapter_name as before_name from rf_elasso1 a left outer join  df_chapter1 b on a.before = b.knowledgeTag1   ")
  temp2 = sqldf("select a.*,b.chapter_name as after_name from temp1 a left outer join df_chapter1 b on a.after = b.knowledgeTag1 ")
  temp3 = sqldf("select before,after, method, before_name, after_name, max(imp1) as imp from temp2 group by before,after, method, before_name, after_name")
  return(temp3)
  
}

get_name <- function(){
  library(sqldf)
  return(read.csv("aihub5_temp.csv"))
}

check_from_merge <- function(lasso, rf, imp_mat){
  lasso$from1 = "LASSO"
  rf$from2 = "RF"
  
  lasso_rf= merge(x=rf,y=lasso, by=c("before","after"), all=TRUE)
  print(paste0("RF ", nrow(rf)," rows and LASSO  ", nrow(lasso), " rows were merged to lasso_rf ", nrow(lasso_rf)," rows"))
  
  lasso_rf$method=ifelse(is.na(lasso_rf$from1) & is.na(lasso_rf$from2),"0",
                         ifelse(is.na(lasso_rf$from1),"RF",
                                ifelse(is.na(lasso_rf$from2),"LASSO",
                                       "BOTH")))
  lasso_rf$from1=NULL
  lasso_rf$from2=NULL
  rf_mat = sqldf("select before, after, max(imp) as imp from imp_mat group by before, after")
  lasso_rf1 = sqldf("select a.*, b.imp as imp1 from lasso_rf a, rf_mat b where a.before = b.before and a.after = b.after") #2022.12.27
  return(unique(lasso_rf1))
  
}


run <- function() {
  
  tic("run")
  
    amath = read_aihub()
    amath1 <- data_cleanse(amath) 
    for (i in 1:length(colnames(amath1))) colnames(amath1)[i]=paste0("KC",colnames(amath1)[i])

#----------------------------------Elastic LASSO + random forest ---------------------------------------  
    tic("Discover relationships b/w KCs based on elastic net results (CV=1000, lasso)")
    
    rel_elasso <- elastic_anal(amath1)

    toc()
    
    tic("run random forest algorithm")
    rf_imp <- run_rf(amath1)
  
    if(rf_cut < 0.3){
      rf_df1 <- sqldf(paste0("select * from rf_imp order by imp desc limit ",as.integer(nrow(rf_imp)*rf_cut)))
    } else {
      rf_df1 <- sqldf(paste0("select * from rf_imp order by imp desc limit ",as.integer(nrow(rel_elasso))))  
    }  
    
    rf_df2 <- clean_birel(rf_df1)
    toc()
    
    df_chapter = get_name()
    
    rf_elasso = check_from_merge(rel_elasso,rf_df2,rf_imp)

    rf_elasso_title = add_title(df_chapter,rf_elasso)

    write.csv(rf_elasso_title,paste0("./OUT/rf_elasso_",date(),".csv"))
    toc()
    
  
}    
  

