

library(LKT)
library(tidyverse)
library(pROC)
library(caret)
library(glmmTMB)
setwd("C:/Users/lukee/Desktop/Projects/2sigma")
val = read.csv("csawesome-datashop-summary.csv")

#Here is where we decide how many attempts until it is coded as incorrect
val$Anon.Student.Id = val$student_id
val$Outcome = ifelse(val$feedback=="Correct","CORRECT","INCORRECT")
val$CF..ansbin. = ifelse(val$Outcome=="CORRECT",1,0)
val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$datetime),format="%Y-%m-%d %H:%M:%S"))
val <- val[order(val$Anon.Student.Id, val$CF..Time.),]

#Making cross-validation folds
unq = sample(unique(val$Anon.Student.Id))
sfold = rep(1:4,length.out=length(unq))
val$fold = rep(0,length(val[,1]))
for(i in 1:4){val$fold[which(val$Anon.Student.Id %in% unq[which(sfold==i)])]=i}

keep_items = which(table((val$content_id))>=10)
keep_items = names(table((val$content_id))[keep_items])
val = val[which(val$content_id %in% keep_items),]

val = computeSpacingPredictors(val, c("KC"))
#Keep rows with skill IDs with > 50 observations

#Add skill tags
tags_auto = read.csv("csawesome-eks-auto-list - csawesome-eks-auto-list.csv")

tags_manual = read.csv("csawesome_eks_weights_manual - csawesome_eks_weights_manual.csv")
content_ids = tolower(unique(tags_manual$content_id))

val$tag_auto = rep(NA,length(val$date))
val$tag_manual = rep(NA,length(val$date))

#Treating combinations of KCs as unique tags. Like in Cognitive Tutor
for(i in 1:length(content_ids)){
  #Sort tags so order tags same across items
  
  idx=which(val$content_id==content_ids[i])
  if(length(idx)>0){
  #Add manual tags
  idx1 = which(tolower(tags_manual$content_id)==content_ids[i])
  val$tag_manual[idx] = paste(tags_manual$skill_id[idx1][order(tags_manual$skill_id[idx1])],collapse="__")
  #Add auto tags
  idx2 = which(tolower(tags_auto$content_id)==content_ids[i])
  tmp = tags_auto$skill_ids[idx2]
  val$tag_auto[idx] = paste(sort(unlist(strsplit(substring(tmp,2,nchar(tmp)-1), split = ","))),collapse="__")
  print(i)
  }
}

val$tag_auto = gsub("'", "", val$tag_auto, fixed = TRUE)

val_trim = val[which(val$tag_manual!="" | val$tag_auto!=""),]

val_trim = computeSpacingPredictors(val_trim, c("tag_auto","tag_manual"))

stu_id = unique(val$Anon.Student.Id)
m1 = LKT(data = setDT(val_trim),
          interc = F,
          components = c("Anon.Student.Id","tag_auto","tag_auto"),
          features = c("propdec","propdec$","intercept"),
          fixedpars=c(.75,.75),
          cv=TRUE)

m1$cv_res

m2 = LKT(data = setDT(val_trim),
         interc = F,
         components = c("Anon.Student.Id","tag_manual","tag_manual"),
         features = c("propdec","propdec$","intercept"),
         fixedpars=c(.75,.75),
         cv=TRUE)

m2$cv_res


fmod_tagauto = LKT(data = setDT(val_trim),
            interc = F,
            components = c("tag_auto"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
            features = c("intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
            fixedpars=c(NA,NA,NA),
            seedpars=c(NA,NA,NA),
            cv=FALSE)
fmod_tagman = LKT(data = setDT(val_trim),
                   interc = F,
                   components = c("tag_manual"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
                   features = c("intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
                   fixedpars=c(NA,NA,NA),
                   seedpars=c(NA,NA,NA),
                   cv=FALSE)
fmod_tagboth = LKT(data = setDT(val_trim),
                  interc = F,
                  components = c("tag_manual","tag_auto"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
                  features = c("intercept","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
                  fixedpars=c(NA,NA,NA),
                  seedpars=c(NA,NA,NA),
                  cv=FALSE)

auc(fmod_tagauto$newdata$CF..ansbin.,fmod_tagauto$prediction)
auc(fmod_tagman$newdata$CF..ansbin.,fmod_tagman$prediction)
auc(fmod_tagboth$newdata$CF..ansbin.,fmod_tagboth$prediction)

fmod1 = LKT(data = setDT(val_trim),
               interc = F,
               components = c("Anon.Student.Id","tag_auto","tag_auto","tag_auto","tag_auto","tag_auto","tag_auto","tag_auto"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
               features = c("propdec","recency$","linesuc$","linefail$","propdec$","logsuc$","logfail","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
               fixedpars=c(.85,.20,.72),
               seedpars=c(NA,NA,NA),
               cv=FALSE)
fmod1$cv_res
confusionMatrix(as.factor(ifelse(fmod1$prediction>.5,1,0)),as.factor(val_trim$CF..ansbin.),positive="1")
library(pROC)
auc(fmod1$newdata$CF..ansbin.,fmod1$prediction)

fmod2 = LKT(data = setDT(val_trim),
            interc = F,
            components = c("Anon.Student.Id","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
            features = c("propdec","recency$","linesuc$","linefail$","propdec$","logsuc$","logfail$","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
            fixedpars=c(.85,.20,.72),
            cv=FALSE)
fmod2$cv_res
confusionMatrix(as.factor(ifelse(fmod2$prediction>.5,1,0)),as.factor(val_trim$CF..ansbin.),positive="1")
library(pROC)
auc(fmod2$newdata$CF..ansbin.,fmod2$prediction)

train.fmod1 = fmod1$newdata[which(fmod1$newdata$fold!=2),]
test.fmod1 = fmod1$newdata[which(fmod1$newdata$fold==2),]


#no skill intercepts, but skill slopes for propdec
#Very simple, but eventually could have a more complex model with more data
fmod1_glmm = glmmTMB(CF..ansbin. ~ 1 +propdecAnon.Student.Id+linesuctag_auto+ linefailtag_auto +propdectag_auto*tag_auto,
                   dat = train.fmod1,family = binomial("logit"),control = glmmTMBControl(optCtrl=list(iter.max=2e4,eval.max=1e6),optArgs=list(method="bobyqa"),profile=TRUE))
summary(mod.pfa1)
pred.pfa1 = predict(mod.pfa1, test.pfa, type = "response")
auc(test.pfa$CF..ansbin., pred.pfa1)  # test AUC: 
confusionMatrix(as.factor(test.pfa$CF..ansbin.),as.factor(ifelse(pred.pfa1>.5,1,0)),positive="1")
sqrt(mean((test.pfa$CF..ansbin. - pred.pfa1)^2))  # test RMSE:



s=summary(mod.pfa1)
write.csv((s$coefficients$cond)[,1],file="model_params.csv")

#This model also has a student level recency proportion measure. It fits much better, however:
#We need to test how often this model might assume mastery of KC2 because KC1 was mastered.
#The level of transfer might be too high and result in strange predictions.
mod.pfa2 = glmmTMB(CF..ansbin. ~ 1 + linefailskill_id +  propdecskill_id + propdecAnon.Student.Id,
                   dat = train.pfa,family = binomial("logit"),control = glmmTMBControl(optCtrl=list(iter.max=2e4,eval.max=1e6),optArgs=list(method="bobyqa"),profile=TRUE))
summary(mod.pfa2)
pred.pfa2 = predict(mod.pfa2, test.pfa, type = "response")
auc(test.pfa$CF..ansbin., pred.pfa2)  # test AUC: 
confusionMatrix(as.factor(test.pfa$CF..ansbin.),as.factor(ifelse(pred.pfa2>.5,1,0)),positive="1")
sqrt(mean((test.pfa$CF..ansbin. - pred.pfa2)^2))  # test RMSE:



#How much do the items matter? Variation of slopes?
fmod3 = LKT(data = setDT(val_trim),
            interc = F,
            components = c("Anon.Student.Id","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
            features = c("propdec","recency","linesuc","linefail","propdec","logsuc","logfail","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
            fixedpars=c(.85,.20,.72),
            cv=FALSE)
auc(fmod3$newdata$CF..ansbin.,fmod3$prediction)




fmod4 = LKT(data = setDT(val_trim),
            interc = F,
            components = c("Anon.Student.Id","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual","tag_manual",
                           "CON_auto","VAR_auto","MOD_auto","IOC_auto","ap_trunc","tag_auto"),
            features = c("propdec","recency","linesuc","linefail","propdec","logsuc","logfail","intercept",
                         "intercept","intercept","intercept","intercept","lineafm"),
            fixedpars=c(.85,.20,.72),
            cv=FALSE)
auc(fmod4$newdata$CF..ansbin.,fmod4$prediction)


fmod5 = LKT(data = setDT(val_trim),
            interc = F,
            components = c("Anon.Student.Id","tag_auto","tag_auto","tag_auto","tag_auto","tag_auto",
                           "tag_auto","tag_auto","ap_trunc","tag_auto"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
            features = c("propdec","recency$","linesuc$","linefail$","propdec$","logsuc$",
                         "logfail","intercept","lineafm","lineafm"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
            fixedpars=c(.85,.20,.72),
            seedpars=c(NA,NA,NA),
            cv=FALSE)
fmod5$cv_res
confusionMatrix(as.factor(ifelse(fmod5$prediction>.5,1,0)),as.factor(val_trim$CF..ansbin.),positive="1")
library(pROC)
auc(fmod5$newdata$CF..ansbin.,fmod5$prediction)

tapply(fmod5$prediction,fmod5$newdata$lineafmap_trunc,function(x){mean(x)})[1:10]
tapply(fmod5$newdata$CF..ansbin.,fmod5$newdata$lineafmap_trunc,function(x){mean(x)})[1:10]


val_trim = computeSpacingPredictors(val_trim, c("ap_trunc"))
fmod5 = LKT(data = setDT(val_trim),
            interc = F,
            components = c("Anon.Student.Id","ap_trunc","ap_trunc","ap_trunc"),
            features = c("propdec","propdec$","recency$","intercept"),
            fixedpars=c(.85,.72,.20),
            cv=FALSE)
auc(fmod5$newdata$CF..ansbin.,fmod5$prediction)


val_trim$cuts = cut(fmod4$prediction,breaks=seq(0,1,.1))

perf_bin = tapply(val_trim$CF..ansbin.,val_trim$cuts,function(x){mean(x)})
pred_bin = tapply(fmod4$prediction,val_trim$cuts,function(x){mean(x)})
plot(perf_bin,xaxt="n",pch=15,xlab = "Bins",main="Model Calibration")
points(pred_bin,col="red",pch=15)
text(2,.7,"Student Performance")
text(2,.8,"Predicted Performance",col="red")
axis(1, at=1:10, labels=names(table(val_trim$cuts)))


val_trim$timecuts = cut(val_trim$ap_truncspacing,breaks=c(0,30,60,600,3600,84600,200000))

perf_bin = tapply(val_trim$CF..ansbin.,val_trim$timecuts,function(x){mean(x)})
pred_bin = tapply(fmod4$prediction,val_trim$timecuts,function(x){mean(x)})
plot(perf_bin,xaxt="n",pch=15,xlab = "Bins",main="Model Calibration over Time bins",ylim=c(0.3,.6))
points(pred_bin,col="red",pch=15)
text(2,.7,"Student Performance")
text(2,.8,"Predicted Performance",col="red")
axis(1, at=1:6, labels=names(table(val_trim$timecuts)))
