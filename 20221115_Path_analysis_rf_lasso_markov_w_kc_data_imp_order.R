##################################################################################################
# The purpose of this program is finding prerequisite relationship of 2KCs w/ lasso+rf+msmm+hmm
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
library(msm)
library(plyr)
library(markovchain)
library(diagram)

init <- function() {
  info <- fromJSON(file = "./PGM/info_back.json")
  
  # host parameters
  useaws <<- info[[1]]$useaws
  bucketname <<- info[[1]]$bucketname
  localpath <<- info[[1]]$localpath

  # csv file name
  kc_title_all_name <<- info[[1]]$kc_title_all_name
  algorithm_first <<- info[[1]]$algorithm_first
  lasso_p_value <<-info[[1]]$lasso_p_value
  rf_cut <<-info[[1]]$rf_cut
  target_file_name <<-info[[1]]$target_file_name
  sm_level <<- info[[1]]$sm_level
  print (useaws)
  print (useaws)
  print (bucketname)
  print (localpath)
  print (kc_title_all_name)
  print (lasso_p_value)
  print (rf_cut)
  print (target_file_name)
  print (algorithm_first)
  print (sm_level)
}  




#-------------------------------------------------------------------------------------------------------------#
#1. define performance data averaging accuracy bu knowre_user_id, learning_id, test, kc_uid
#-------------------------------------------------------------------------------------------------------------#


make_series_table <- function(dat1) {
  
  print(paste0("student records ",length(unique(dat1$knowre_user_id))))
  
  temp=sqldf("select knowre_user_id, min(learning_id) as learning_id from dat1 group by knowre_user_id")

  df = sqldf("select knowre_user_id, learning_id, test, KC_uid as kc_uid, avg(accuracy*1.0) as accuracy from dat1 where knowre_user_id||learning_id in (select knowre_user_id||learning_id from temp) group by knowre_user_id, learning_id, test, kc_uid")
  
  print(paste0("student records took more than once ",length(unique(df$knowre_user_id))))
  
  return(df)
}

#-------------------------------------------------------------------------------------------------------------#
#2. make a markov table with students took twice who didn't go back
#-------------------------------------------------------------------------------------------------------------#

make_markov_table <- function(dat1) {
  
  print ("start markov_df")
  
  # #of students
  print(paste0("# of students :",length(unique(dat1$knowre_user_id)), " records"))

  # students who took twice   
  temp = sqldf("select knowre_user_id, max(test) as max_test from dat1 group by knowre_user_id") 
  temp2 = sqldf("select knowre_user_id from temp where max_test=2")
  dat11=sqldf("select  knowre_user_id, learning_id, test, kc_uid,problem_id, accuracy from dat1 where knowre_user_id in (select * from temp2)")
  print(paste0("# of students who took twice :",length(unique(dat11$knowre_user_id)), " records"))

  # remove students took tests because of backing
  t2 = sqldf("select knowre_user_id,test,count(distinct learning_id) as cnt from dat11 group by knowre_user_id,test")
  dat12 = sqldf("select knowre_user_id, test, KC_uid as kc_uid,accuracy from dat11  where knowre_user_id not in (select distinct knowre_user_id from t2 where cnt >=2) group by knowre_user_id, test, kc_uid")

  # # of students took 2 tests
  print(paste0("# of students who took twice not backed :",length(unique(dat12$knowre_user_id)), " records"))

  return(dat12)
}

#-------------------------------------------------------------------------------------------------------------#
#3. make a wide table differentiating test information
#-------------------------------------------------------------------------------------------------------------#

make_markov_reshaped_table  <- function(dat12){
  library(reshape2)
  dat2=as.data.frame(acast(dat12,knowre_user_id+test~ kc_uid, value.var="accuracy", fun=mean))
  temp=str_split(rownames(dat2),"_",simplify = TRUE)
  dat3 = cbind(temp, dat2)
  colnames(dat3)[1:2] = c("knowre_user_id","test")
  return(dat3)
}

#-------------------------------------------------------------------------------------------------------------#
#4. make a hmm table with 2 binary states 
#-------------------------------------------------------------------------------------------------------------#

trans_for_hmm <- function(dat){

  dat1 = dat[dat$test==1,]
  dat2 = dat[dat$test==2,]
  
  dat3 = merge(x=dat1, y=dat2, by = c("knowre_user_id", "kc_uid"))[,c("knowre_user_id","kc_uid","accuracy.x","accuracy.y")]
  colnames(dat3)=c("knowre_user_id","kc_uid","acc1","acc2")
  
  dat3$acc1 = ifelse(as.numeric(dat3$acc1)>0.6,1,0)
  dat3$acc2 = ifelse(as.numeric(dat3$acc2)>0.6,1,0)
  
  dat4=sqldf("select knowre_user_id, kc_uid,avg(acc1) as acc1, avg(acc2) as acc2 from dat3 group by knowre_user_id, kc_uid")
  print(paste0(length(unique(dat4$knowre_user_id))," records with 2 accuracies from 2 tests were returned"))
  
  return(dat4)
}

#-------------------------------------------------------------------------------------------------------------#
#5. make a hmm table
#-------------------------------------------------------------------------------------------------------------#

trans_for_hmm2 <- function(dat){

  dat1=sqldf("select knowre_user_id,learning_id, kc_uid,avg(accuracy) as accuracy from dat group by knowre_user_id,learning_id, kc_uid")
  return(dat1)
}

#-------------------------------------------------------------------------------------------------------------#
#5. make a wide table obs : knowre_user_id, columns : kc_uid
#-------------------------------------------------------------------------------------------------------------#

make_reshaped_table  <- function(dat){
  dat1=as.data.frame(acast(dat,knowre_user_id ~ kc_uid,value.var="accuracy", fun=mean))
 return(dat1)
}

#-------------------------------------------------------------------------------------------------------------#
#6. make a wide table obs : knowre_user_id, learning_id, test, columns : kc_uid
#-------------------------------------------------------------------------------------------------------------#

make_reshaped_table2  <- function(dat12){
  library(reshape2)
  dat2=as.data.frame(acast(dat12,knowre_user_id+learning_id+test~ kc_uid, value.var="accuracy", fun=mean))
  temp=str_split(rownames(dat2),"_",simplify = TRUE)
  dat3 = cbind(temp, dat2)
  colnames(dat3)[1:3] = c("knowre_user_id","learning_id","test")
  return(dat3)
}

#-------------------------------------------------------------------------------------------------------------#
#6. make a wide table obs : knowre_user_id, test, columns : kc_uid
#-------------------------------------------------------------------------------------------------------------#

make_markov_reshaped_table  <- function(dat12){

#  dat12=markov_series
  library(reshape2)
  dat2=as.data.frame(acast(dat12,knowre_user_id+test~ kc_uid, value.var="accuracy", fun=mean))
  temp=str_split(rownames(dat2),"_",simplify = TRUE)
  dat3 = cbind(temp, dat2)
  colnames(dat3)[1:2] = c("knowre_user_id","test")
  return(dat3)
}

#-------------------------------------------------------------------------------------------------------------#
#7. fitting hmm
#-------------------------------------------------------------------------------------------------------------#

hmm2 <- function(dat1){

  dat_alphabet <- c("0", "1") 
  dat_labels <- c("Incorrect","Correct")
  dat_scodes <- c("0","1") 
  dat_seq <- seqdef(dat1, 1:2, alphabet = dat_alphabet, states = dat_scodes, labels = dat_labels, xtstep = 1)
  
  initial_probs <- c((length(dat1[,1])-sum(dat1[,1]))/length(dat1[,1]), sum(dat1[,1])/length(dat1[,1]))
  
  trans <- matrix( c(0.7, 0.3, 0.3, 0.7), nrow = 2, ncol = 2, byrow = TRUE)
  trans <- seqtrate(dat_seq, c(0,1))
  trans <- trans/rowSums(trans)  
  print("here 3")
  emiss <- matrix( c(0.98, 0.02,0.02, 0.98), nrow = 2, ncol = 2, byrow = TRUE)
  tryCatch({
    init_hmm <- build_hmm(observations = dat_seq, transition_probs = trans, emission_probs = emiss, initial_probs = initial_probs)
    fit_hmm <- fit_model(init_hmm, control_em = list(restart = list(times = 100))) 
    hmm_dat <- fit_hmm$model
    return(hmm_dat)
  },error=function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  }) 
}

#-------------------------------------------------------------------------------------------------------------#
#9. make a table w/ appropriate rows of found relationships from data under LASSO + RF
#-------------------------------------------------------------------------------------------------------------#

comparison_w_target2 <- function(dat, test_kc){
  test_kc=colnames(regression_data1)
  dat=rf_elasso
  dat$diff = as.integer(str_sub(dat$after,-2,-1)) - as.integer(str_sub(dat$before,-2,-1))
# 2022.06.18 dat0 = dat[dat$diff < 6,]  
  dat0=dat
#----------------------------------change OK ---------------------------------------  
  target0=read.csv(target_file_name)
  colnames(target0)=c("before","after")
  test_kc = data.frame(test_kc)
  target=sqldf(paste0("select rightstr(before,4) as before, rightstr(after,4) as after, 1 as mapped from target0 where leftstr(after,2) like '",sm_level,"%' and rightstr(before,4) in (select * from test_kc) and rightstr(after,4) in (select * from test_kc) and leftstr(before, 2)=leftstr(after,2)"))
  
  if(algorithm_first=='R'){ 
    dat3 = first_rf(dat0,target)
  } else {
    dat3 = first_lasso(dat0,target)
  }
  rela_mapped = merge(x=dat3,y=target, by=c("before","after"), all.x=TRUE)
  rela_mapped[is.na(rela_mapped)]=0

  r=round((sum(rela_mapped$mapped)/nrow(rela_mapped))*100,2)
  print(paste0("level : ", sm_level, " Target : ", nrow(target),",  data : ", nrow(dat3), ",   mapped : ", round(r), "%" ))
  return(rela_mapped)
  
}                                    


first_rf <- function(dat,target){

  dat$ord = ifelse(dat$method=='BOTH',1,ifelse(dat$method=='RF',2,3))  
  dat1 = sqldf(paste0("select * from dat order by ord , imp desc limit ", nrow(target)))
  
  return(dat1)  
}

first_lasso <- function(dat,target){
  
  dat$ord = ifelse(dat$method=='BOTH',1,ifelse(dat$method=='LASSO',2,3))  
  dat1 = sqldf(paste0("select * from dat order by ord , imp desc limit ", nrow(target)))

return(dat1)  
}

#-------------------------------------------------------------------------------------------------------------#
#10. Call HMM function conditioning 1st test was wrong
#-------------------------------------------------------------------------------------------------------------#


cal_hmm_prob2 <- function(dat, candidates){

#  dat=hmm_data
#  candidates=relation_f
  
  candidates1=candidates[,c("before","after","title1","title2","method","imp","mapped")]
  candidates2=candidates[,c("after","before","title2","title1","method","imp","mapped")] 
  colnames(candidates2)=colnames(candidates1)
  candidates1$reverse = 0
  candidates2$reverse = 1
  
  candidates = rbind(candidates1, candidates2)

  kc_prob = data.frame(KC1=character(),
                       KC2=character(), 
                       title1=character(),
                       title2=character(),
                       method=character(),
                       imp=numeric(),
                       mapped=numeric(),
                       FT00=numeric(),
                       FT01=numeric(),
                       FT10=numeric(),
                       FT11=numeric()) 
  
  
  for(i in 1:nrow(candidates) ){

      print(paste0(candidates[i,1]," ",candidates[i,2]))

    sql = paste0("select knowre_user_id, kc_uid, acc2 as accuracy from dat where (kc_uid='",candidates[i,1],"' or kc_uid='",candidates[i,2],"') and knowre_user_id in (select knowre_user_id from dat where kc_uid = '",candidates[i,1],"' and acc1=0)")
    dat1 = sqldf(sql)
    dat2 <- make_reshaped_table(na.omit(dat1))
    print("here 1")
    hm_prob <- hmm2(dat2)
    print("here 2")
    

    r=cbind(candidates[i,1],candidates[i,2],candidates[i,3],candidates[i,4],candidates[i,5],candidates[i,6],candidates[i,7],hm_prob$transition_probs[1,1],hm_prob$transition_probs[1,2],hm_prob$transition_probs[2,1],hm_prob$transition_probs[2,2])
    if(exists("r") & length(r)==ncol(kc_prob))kc_prob = rbind(kc_prob,r)

  } 
  
  return(kc_prob)
}

#-------------------------------------------------------------------------------------------------------------#
#11. make a 1/2 table with FT00, FT01, FT10, FT11, RFT11 information
#-------------------------------------------------------------------------------------------------------------#

make_one_table <- function(dat){
 
  dat$diff = as.integer(substr(dat$after,3,4))-as.integer(substr(dat$before,3,4))  
  d1=dat[dat$diff>0,]  
  d2=dat[dat$diff<0,]
  #d1=dat[1:(nrow(dat)/2),]
  #d2=dat[((nrow(dat)/2)+1):nrow(dat),]
 d1$concat = paste0(d1$before,d1$after)
 d2$concat = paste0(d2$after, d2$before)
 dat1=merge(x=d1, y=d2, by="concat")
 
 dat2=dat1[,c("before.x","after.x","title1.x","title2.x","method.x","imp.x","mapped.x","FT00.x","FT01.x","FT10.x","FT11.x","FT00.y","FT01.y","FT10.y","FT11.y")]
 colnames(dat2)=c("before","after", "title1","title2","method","imp","mapped","FT00","FT01","FT10","FT11","TF00","TF01","TF10","TF11")          
 dat3=dat2[,c("before","after", "title1","title2","method","imp","mapped","FT00","FT01","FT10","FT11","TF11")]

 dat3$FT00=round(as.numeric(dat3$FT00),2)
 dat3$FT01=round(as.numeric(dat3$FT01),2)
 dat3$FT10=round(as.numeric(dat3$FT10),2)
 dat3$FT11=round(as.numeric(dat3$FT11),2)
 dat3$TF11=round(as.numeric(dat3$TF11),2)

  dat4 <- data.frame(lapply(dat3,    # Using Base R functions
                                  function(x) if(is.numeric(x)) round(x, 2) else x))

   dat5 = dat4[,c("before","after", "title1","title2","mapped","method","FT00","FT01","FT10","FT11","TF11")]
   dat6 = sqldf("select before,after,title1, title2, mapped, method, min(FT00) as FT00,min(FT01) as FT01,min(FT10) as FT10,min(FT11) as FT11,min(TF11) as TF11 from  dat5 group by before,after,title1, title2, mapped, method")
   dat6$diff = as.integer(substr(dat6$after,3,4))-as.integer(substr(dat6$before,3,4))
   
   
#   return(dat6[dat6$diff<6,])
   return(dat6)
} 

#-------------------------------------------------------------------------------------------------------------#
#12. note where the rule(=relation) was from 
#-------------------------------------------------------------------------------------------------------------#

check_from_merge <- function(lasso, rf){


  lasso$from1 = "LASSO"
  rf$from2 = "RF"
  
  lasso_rf= merge(x=rf,y=lasso, by=c("before","after"), all=TRUE)
  
  lasso_rf$method=ifelse(is.na(lasso_rf$from1) & is.na(lasso_rf$from2),"0",
                        ifelse(is.na(lasso_rf$from1),"RF",
                               ifelse(is.na(lasso_rf$from2),"LASSO",
                                      "BOTH")))
  lasso_rf$from1=NULL
  lasso_rf$from2=NULL
  
  return(lasso_rf)
  
}

#-------------------------------------------------------------------------------------------------------------#
#13. add title to random forest output
#-------------------------------------------------------------------------------------------------------------#

add_title_rf <- function(dat){
  dat1 = sqldf("select a.*, b.kc_title as title1 , c.kc_title as title2 from dat a , kc_title b , kc_title c where a.before = b.kc_uid and a.after = c.kc_uid")
  return(dat1)
}

#-------------------------------------------------------------------------------------------------------------#
#13. Since there is no order, birelation is not meaningful
#-------------------------------------------------------------------------------------------------------------#

clean_birel <- function(dat){
#  dat$reverse = ifelse(paste0(dat$after,dat$before) %in% paste0(dat$before,dat$after), 2,1)
#  dat$diff = as.integer(substr(dat$after,3,4))-as.integer(substr(dat$before,3,4))
#  dat1 =rbind(dat[dat$diff > 0 ,],dat[dat$diff<0 & dat$reverse ==1,])
#  dat2=sqldf("select min(before, after) as before, max(before,after) as after,imp from dat1 order by before, after")
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

#-------------------------------------------------------------------------------------------------------------#
#15. calculate cut-off value w/ rpart algorithm
#-------------------------------------------------------------------------------------------------------------#

hmm_cut <-function(dat){
  library(rpart)
#  tree_model <- rpart(mapped ~ H_FT00+H_TF11, data=dat, control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
  tree_model <- rpart(mapped ~ FT00+TF11, data=dat, control=rpart.control(minsplit=2, minbucket=1, cp=0.001))  
#  cut = tree_model$split[row.names(tree_model$split)=="H_FT00","index"][1]
  cut = tree_model$split[row.names(tree_model$split)=="FT00","index"][1]
  print(paste0("Cutoff of H_FT00 & H_TF11 :",cut))
  return(cut)
}  

#-------------------------------------------------------------------------------------------------------------#
#16. note that the relation is forward or not
#-------------------------------------------------------------------------------------------------------------#

add_cart_hmm_direction <-function(dat){
#if(length(hmm_cut(dat))==0){dat$hmm_direction = "."  } else {
#    dat$hmm_direction = ifelse(dat$FT00 > hmm_cut(dat),ifelse(dat$TF00 > hmm_cut(dat),"forward","."),".")
#    dat$hmm_direction = ifelse(is.na(dat$hmm_direction),ifelse(is.na(dat$FT00),',',"forward"),dat$hmm_direction)
#}    
  dat$hmm_direction=ifelse(dat$FT00 >=0.3 , ifelse(dat$TF11 >= 0.3,"forward","."),".")
  return(dat)
}

#-------------------------------------------------------------------------------------------------------------#
#16. make series information for msmm analysis
#-------------------------------------------------------------------------------------------------------------#

make_msm_series <- function(dat){
  
  
  colnames(dat)=c("knowre_user_id","test","before","after")
  selected = dat[dat$test==1 & dat$before==0,] #select records of test1 among students who got a score "inaccurate"
  
  
  dat1=sqldf("select * from dat where test=2 and knowre_user_id in (select knowre_user_id from selected)")
  
  
  # change 0~1 to 0 if less than 1 and else 1
  # _test1__test2_________    
  dat1[,5] = ifelse(dat1[,3] <0.6,0,1);                                                           #| 0 | 0 | 0(2) | 0(4) |
  dat1[,6] = ifelse(dat1[,4] <0.6,0,1);                                                           #|   |   |      | 1(5) |
  colnames(dat1)=c("knowre_user_id","test","kc1","kc2","before","after")                          #|   | 1 | 1(3) | 0(6) |
  #|   |   |      | 1(7) |
  dat1$before_state = ifelse(dat1$before==0, 2,3)                                                 #|kc1|kc2|kc1   |kc2   |  
  dat1$after_state = ifelse(dat1$before==0,ifelse(dat1$after==0,4,5),ifelse(dat1$after==0,6,7))
  target=melt(dat1, id.vars="knowre_user_id", measure.vars=c("before_state", "after_state"))
  colnames(target)=c("knowre_user_id","time","state")
  target$time = ifelse(target$time=="before_state",2,3) 
  
  temp=cbind(selected$knowre_user_id,rep(1,nrow(selected)),rep(1,nrow(selected))) 
  colnames(temp)=colnames(target)
  
  target = rbind(temp, target)
  target[,1]=as.integer(target[,1])
  target[,2]=as.integer(target[,2])
  target[,3]=as.integer(target[,3])
  print(table(target$time,target$state))
  return(target)
} 

#-------------------------------------------------------------------------------------------------------------#
#17. Run MSMM
#-------------------------------------------------------------------------------------------------------------#

run_msm <- function(msm_data){
  #  msm_data = msm_2nd_test
  msm_data1=msm_data[order(msm_data$knowre_user_id,msm_data$time),]
  
  # Node changes from 1 to (3,4) and 2 to (5,6) are just possible. So, just the information was extracted from table commend.
  
  
  trans <- data.frame(rbind(c(1,2), c(1,3), c(2,4), c(2,5), c(3,6),  c(3,7)))
  colnames(trans) =c("f1", "t1")
  
  temp=sqldf("select time, state,count(*) as freq from msm_data1 group by time, state")
  
  q=merge(x=trans, y=temp, by.x="t1", by.y="state", all.x=TRUE)
  
  temp2 = sqldf("select f1, sum(freq) as sum_freq from q group by f1")
  
  q1 = merge(x=q, y=temp2, by = "f1", all.x=TRUE)
  
  q1[is.na(q1)]=0
  q1$ratio=(q1$freq*1.0)/(q1$sum_freq*1.0)
  
  
  
  # Initialization of q matrix 
  M=matrix(0,nrow=4,ncol=7, byrow=TRUE); diag(M)<-1
  
  q2=rbind(c(0,q1[1,ncol(q1)],q1[2,ncol(q1)],0,0,0,0),c(0,0,0,q1[3,ncol(q1)],q1[4,ncol(q1)],0,0),c(rep(0,5),q1[5,ncol(q1)],q1[6,ncol(q1)]),M)
  # Run MSM
  pmat = matrix(0,nrow=7,ncol=7, byrow=TRUE)
  
  tryCatch({
    
    summ_table.msm <- msm( state ~time, subject=knowre_user_id, data = msm_data1, qmatrix = q2,control=list(fnscale=4000,maxit=10000),gen.inits=TRUE)
    pmat = as.matrix(pmatrix.msm(summ_table.msm,4))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #  return(pnext.msm(summ_table.msm))
  init_pmat = list(q1,pmat)
  
  return(init_pmat)
  
}

#-------------------------------------------------------------------------------------------------------------#
#18. Generate a graph according to the msm result
#-------------------------------------------------------------------------------------------------------------#

trans_m_to_graph <- function(tmA,kc1,kc2){
  
  # create graphs according to the information on nodes
  
  stateNames <- c("Start",paste0(kc1,"=0"),paste0(kc1,"=1"),paste0(kc2,"=0"),paste0(kc2,"=1"))
  row.names(tmA) <- stateNames; colnames(tmA) <- stateNames
  
  plotmat(tmA, pos = c(1, 2,  2), curve = 0, name = stateNames,
          lwd = 2, box.lwd = 2, cex.txt = 0.8,
          box.type = "circle", box.prop = 0.3,  arr.type = "none", 
          arr.pos = 0.4, shadow.size = 0.01,         self.cex = .2, 
          self.shifty = 0.05,
          self.shiftx = -0.01,
          main = paste0("relations : ", kc1 ," and ", kc2) )      
}

#-------------------------------------------------------------------------------------------------------------#
#19. Generate a graph according to the msm result
#-------------------------------------------------------------------------------------------------------------#

msm_clean <- function(msm1){
  msm2 = matrix(0,nrow=5, ncol=5, byrow=TRUE)
  for(i in 1:5) for(j in 1:5) msm2[i,j]=msm1[i,j]
  
  tryCatch({
    
    msm2[1,4] = msm1[1,4]+ msm1[1,6]
    msm2[1,5] = msm1[1,5]+ msm1[1,7]
    msm2[3,4] = msm1[3,6]
    msm2[3,5] = msm1[3,7]    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #  return(pnext.msm(summ_table.msm))
  
  
  return(round(msm2,2))
  
}

#-------------------------------------------------------------------------------------------------------------#
#20. MSMM analysis
#-------------------------------------------------------------------------------------------------------------#

msm_anal <-function(dat){
  dat=analysis_data
  combos1 <- combn(ncol(dat)-2,2)
  combos2 <- rbind(combos1[2,],combos1[1,])
  combos = cbind(combos1, combos2)
  
  file.remove("kc_relations_time.csv")
  
  transition_matrix <- apply(combos,2,function(x) {  
    msm_2nd_test <- make_msm_series(analysis_data[,c(1,2,x[1]+2,x[2]+2)])
    print(paste(colnames(analysis_data)[x[1]+2],"  ",colnames(analysis_data)[x[2]+2]))
    colnames(msm_2nd_test)=c("knowre_user_id","time","state")
    
    msm1 <- run_msm(msm_2nd_test)
    init_mat=msm1[[1]]
    estimated = msm1[[2]]
    print(estimated)
    msm2 <- msm_clean(estimated)
    #create graphs according to information from a transition matrix and KC names  
    #trans_m_to_graph(round(msm2,2),colnames(analysis_data)[x[1]+2],colnames(analysis_data)[x[2]+2])
    
    x=data.frame(colnames(analysis_data)[x[1]+2],colnames(analysis_data)[x[2]+2],msm2[2,4],msm2[2,5],msm2[3,4],msm2[3,5])
    write.table(x, file = "kc_relations_time.csv", sep = ",", append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)
    
    return()
  })
  toc()
  
  kc_relations=read.csv("kc_relations_time.csv")  
  colnames(kc_relations) = c("before","after","FT00","FT01","FT10","FT11")
  return(kc_relations)
}

#-------------------------------------------------------------------------------------------------------------#
#21. make a 1/2 table with FT00, FT01, FT10, FT11, RFT11 
#-------------------------------------------------------------------------------------------------------------#

clean_msm_res <- function(msm_res){
  # Owing to that msmm calculates probabilities for all possible couples such as (KC01,KC03) and (KC03,KC01), we can use RFT from the later.
  msm_res$diff = as.integer(substr(msm_res$after,3,4))-as.integer(substr(msm_res$before,3,4))
# 2022.06.18 res1 = unique(msm_res[msm_res$diff > 0 & msm_res$diff < 6,]);res2 = unique(msm_res[msm_res$diff < 0 & msm_res$diff > -6,])
  res1 = unique(msm_res[msm_res$diff > 0,]);res2 = unique(msm_res[msm_res$diff < 0,])  
  res1$normal = paste0(res1$before,res1$after);res1$reverse = paste0(res1$after,res1$before)
  res2$normal = paste0(res2$before, res2$after);res2$reverse = paste0(res2$after,res2$before) 
  
  res3=sqldf("select distinct a.*, b.FT11 as RFT11 from res1 a inner join res2 b on a.normal = b.reverse")
  res3$normal = NULL
  res3$reverse = NULL
  return(res3)
}

#-------------------------------------------------------------------------------------------------------------#
#22. note direction according to the probabilities calculated by msmm
#-------------------------------------------------------------------------------------------------------------#

add_msm_direction <- function(msm_res){
  
  msm_res$m_direction = ifelse(msm_res$FT00 >= 0.3 & msm_res$RFT11 >= 0.3 ,"forward","."  )

  return(msm_res)
}

#-------------------------------------------------------------------------------------------------------------#
#23. merge msmm , random forest, elastic lasso result
#-------------------------------------------------------------------------------------------------------------#

merge_msm_rf_elasso <- function(msm_res, rf_lasso_res){
#  msm_res=m_relation2
#  rf_lasso_res =rf_elasso
  
  res <- merge(x=msm_res, y=rf_lasso_res, by= c("before","after"))
  res1 = res[,c("before","after","title1","title2","method","m_direction","FT00","FT01","FT10","FT11","RFT11")]
  return(res1)
}

#-------------------------------------------------------------------------------------------------------------#
#8 data cleansing  with 4 rules (1.correct all kc, 2.incorrect all kc, 3 correct all obs, 4. incorrect all obs)
#-------------------------------------------------------------------------------------------------------------#


data_cleanse<- function(data){
  data<-data[,-1]
  data_clean <- data[rowSums(data)> 0 & rowSums(data)<ncol(data), colSums(data[]) !=nrow(data[]) & colSums(data[])!=0] # delete if low count
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


run <- function() {
  
  tic("run")
  
  tic("KC title all")
#----------------------------------change OK ---------------------------------------  
  kc_title_all=read.csv(kc_title_all_name, fileEncoding = 'euc-kr',encoding='cp949')
  kc_title_all$sm_level=as.integer(word(kc_title_all$kc_uid,1,sep = "-"))
  toc()
  sm_level = sm_level
# FOR
    library(sqldf)
  
    kc_title <- sqldf(paste0("select rightstr(kc_uid,4) as kc_uid, title as kc_title from kc_title_all where leftstr(kc_uid,2) like '", sm_level,"%'"))
  
#----------------------------------change OK---------------------------------------  
    smath = read.csv(paste0("./DATA/kc_dedup_smath",sm_level,".csv"))
    
    smath$temp=substr(smath$test,15,15)
    smath$test=smath$temp
    smath$temp=NULL
#----------------------------------data cleaning ----------------------------------

    tic("make series table w/o considering # of tests. We just want to find the relationship b/w KCs")
    series_data <- make_series_table(smath) 
    toc()

    tic("make a table for elastic lasso and random forest")
    regression_data <- make_reshaped_table(series_data[series_data$test==1,]) # make a horizontally wide table "knowre_user_id~ kc_uid in terms of test1"
#    temp <- data_cleanse(regression_data)   
#    regression_data1 <- drop_high_corr(temp)
     regression_data1 <- data_cleanse(regression_data) 
    toc() 
    
      
    tic("make series table and markov table with 2 test results w/o backing information")    
    markov_series = make_markov_table(smath) 
    analysis_data <- make_markov_reshaped_table(markov_series) # make a horizontally wide table for msmm
    toc()
  
    tic("make a table with columns knowre_user_id, kc_uid, test1(accuracy), test2(accuracy)")
    hmm_data = trans_for_hmm(markov_series)
    toc()
    
#----------------------------------Elastic LASSO + random forest ---------------------------------------  
    tic("Discover relationships b/w KCs based on elastic net results (CV=1000, lasso)")
    rel_elasso <- elastic_anal(regression_data1)
    toc()
  
    tic("run random forest algorithm")
    rf_imp <- run_rf(regression_data1)
  
    if(rf_cut < 0.3){
      rf_df1 <- sqldf(paste0("select * from rf_imp order by imp desc limit ",as.integer(nrow(rf_imp)*rf_cut)))
    } else {
      rf_df1 <- sqldf(paste0("select * from rf_imp order by imp desc limit ",as.integer(nrow(rel_elasso))))  
    }  
    
    rf_df2 <- clean_birel(rf_df1)
    rf_df3 <- add_title_rf(rf_df2)
    toc()
    
    tic("merge LASSO and RF")
    rf_elasso<- merge(x=add_title_rf(check_from_merge(rel_elasso, rf_df3)[-1,c("before","after","method")]),y=rf_imp, by=c("before","after"),all.x=TRUE)                                                                                                                          
    
    toc()
    
    relation_f <- comparison_w_target2(rf_elasso,colnames(regression_data1))
#----------------------------------------msmm-----------------------------------------------------------------
#    tic("msm_run")
#    kc_msm_relations <- msm_anal(analysis_data)
#    m_relation=clean_msm_res(kc_msm_relations) 
#    m_relation2 = add_msm_direction(m_relation)
#    toc() 
    
#    tic("merge msm with rf_elasso")  
#    msm_rf_elasso <- merge_msm_rf_elasso(m_relation2, rf_elasso)
#    write.csv(msm_rf_elasso,paste0("./OUT/MSMM_",sm_level,".csv"))
#    toc()  
    
#---------------------------------------HMM-----------------------------------------------------------------  
    tic("Calculate HMM probabilities for rel_elasso1 in terms of 2KC")
    rela_hmm <- cal_hmm_prob2(hmm_data,relation_f)
    colnames(rela_hmm)=c("before","after","title1","title2","method","imp","mapped","FT00","FT01","FT10","FT11")
    toc()
    
  
    tic("Make transition graphs for all relations( ex) KC01 and KC03")
    rela_hmm_rel <- make_one_table(rela_hmm)
    toc()
    
    relf <- add_cart_hmm_direction(rela_hmm_rel)  
    write.csv(relf,paste0("./OUT/HMM_",sm_level,".csv"))
#  }
}    
  

