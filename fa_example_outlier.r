#FA concrete example for unit 1

#Most co-occurring items in unit 1 within student?

chapter="Unit1-Getting-Started"
subchapter="topic-1-2-java-intro"
tag=names(rev(sort(table(val_trim$tag_auto[which(val_trim$chapter==chapter)]))))[1]
chosen_items = names(rev(sort(table(val_trim$content_id[which(val_trim$chapter==chapter & val_trim$tag_auto==tag)]))))[1:5]
chosen_items = names(rev(sort(table(val_trim$content_id[which(val_trim$subchapter==subchapter)]))))[1:5]
#For each student get their performance on the chosen items
stu_id = unique(val_trim$student_id[which(val_trim$subchapter==subchapter & val_trim$content_id %in% chosen_items)])

stu_perf = matrix(nrow=length(stu_id),ncol=length(chosen_items))
colnames(stu_perf) = chosen_items
for(i in 1:length(stu_id)){
print(i)  
idx = which(val_trim$student_id == stu_id[i] & val_trim$content_id %in% chosen_items & val_trim$chapter==chapter)
res = tapply(val_trim$CF..ansbin.[idx], val_trim$content_id[idx],function(x){mean(x,na.rm=TRUE)})  
stu_perf[i,match(names(res),colnames(stu_perf))] = res


}
#drop any that have any isna
stu_perf = stu_perf[apply(stu_perf,1,function(x){all(!is.na(x))}),]

#Efa on these items, one factor
library(psych)
fa1 = fa(stu_perf[,c(1,2,3,4,5)],1)
fa1
