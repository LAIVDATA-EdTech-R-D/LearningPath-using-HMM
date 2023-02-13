library(sqldf)

aihub1 = read.csv("./DATA/(merged)1st_all_utf_8_sig.csv") 

temp11 = sqldf("select knowledgeTag, chapter_id, chapter_name, count(distinct learnerID)  as no_std from aihub1 group by knowledgeTag")
temp12 = sqldf("select rank() over (order by no_std desc) as row_rank, chapter_name, chapter_id, knowledgeTag, no_std from temp11 ")


library(reshape2)

aihub_sel = sqldf("select * from aihub1 where knowledgeTag in (select knowledgeTag from temp12) and (chapter_id > 13 and chapter_id < 21) ")
aihub_sel2 = sqldf("select *, ROW_NUMBER() OVER(PARTITION BY learnerID, knowledgeTag) as rown FROM aihub_sel ")
aihub_sel3 = sqldf("select * from aihub_sel2 where rown = 1")


dat1=as.data.frame(acast(aihub_sel,learnerID~ knowledgeTag, value.var="answerCode", fun=mean))
aihub.complete <- dat1[complete.cases(dat1) ,] 

nrow(aihub.complete)
print(" rows are complete")
write.csv(aihub.complete,"grade1_14_20_덧셈과뺄셈_all.csv")
dat1=aihub.complete[rowSums(aihub.complete)<7 & rowSums(aihub.complete)>0,]
write.csv(dat1, "grade1_14_20_덧셈과뺄셈.csv")
colSums(is.na(dat1))
grade1_14_20_덧셈과뺄셈_1st.csv
return(aihub.complete)

aihub_sel = sqldf("select * from aihub1 where knowledgeTag in (select knowledgeTag from temp12) and (chapter_id > 13 and chapter_id < 21) ")
dat1=as.data.frame(acast(aihub_sel3,learnerID~ knowledgeTag, value.var="answerCode", fun=mean))
aihub.complete <- dat1[complete.cases(dat1) ,] 
write.csv(aihub.complete,"grade1_14_20_덧셈과뺄셈.csv")
