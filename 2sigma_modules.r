#Notes
#Make these modules

#What columns in input? Outcome, tags, timestamp, student id, some decision about how the tags are in the dataset.

#Input data => output model fit stats. LASSOLKT, PFA, basic IRT reporting significance of tags (per tag). Relative importance index figure. Output: list of non-significant tags.
#Covariance clustering -> How much does it change versus input clusters/tags? Output: cluster-tag IDs. What tags are in each cluster


#Make it iterative, share github results with Vishal as make progress.


###Load libraries


###LOad data



### Fit a few models



###Item analysis
fmod_tagauto = LKT(data = setDT(val_trim),
                   interc = F,
                   components = c("Anon.Student.Id","tag_auto"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
                   features = c("propdec","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
                   fixedpars=c(.8,NA,NA),
                   seedpars=c(NA,NA,NA))
fmod_tagman = LKT(data = setDT(val_trim),
                  interc = F,
                  components = c("Anon.Student.Id","tag_manual"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
                  features = c("propdec","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
                  fixedpars=c(.8,NA,NA),
                  seedpars=c(NA,NA,NA))
fmod_tagboth = LKT(data = setDT(val_trim),
                   interc = F,
                   components = c("Anon.Student.Id","tag_manual","tag_auto"),#,"tag_auto","tag_auto","tag_auto","tag_auto","Anon.Student.Id","tag_auto","Anon.Student.Id","tag_auto"),
                   features = c("propdec","intercept","intercept"),#"logfail","intercept","propdec","propdec","logitdec","logitdec","lineafm"),
                   fixedpars=c(.8,NA,NA),
                   seedpars=c(NA,NA,NA))

auc(fmod_tagauto$newdata$CF..ansbin.,fmod_tagauto$prediction)
auc(fmod_tagman$newdata$CF..ansbin.,fmod_tagman$prediction)
auc(fmod_tagboth$newdata$CF..ansbin.,fmod_tagboth$prediction)



#RMSE by item by tag

rmse_auto = tapply(fmod_tagauto$prediction-fmod_tagauto$newdata$CF..ansbin.,fmod_tagauto$newdata$content_id,function(x){sqrt(mean(x^2))})
rmse_man = tapply(fmod_tagman$prediction-fmod_tagman$newdata$CF..ansbin.,fmod_tagman$newdata$content_id,function(x){sqrt(mean(x^2))})
rmse_diff = abs(rmse_auto-rmse_man)
bp = boxplot(rmse_diff)
#Biggest differences in fit across tags

bp$out[order(bp$out)]

#Items that both tags fit badly
names(which(rmse_auto>.5 & rmse_man>.5))


#Item discrim

content_ids = unique(val_trim$content_id)
item_disc = data.frame(content_id = content_ids,
                       upper = rep(NA,length(content_ids)),
                       lower = rep(NA,length(content_ids)),
                       discrim = rep(NA,length(content_ids)),
                       N = rep(NA,length(content_ids)),
                       item_perf = rep(NA,length(content_ids)),
                       item_pred = rep(NA,length(content_ids)),
                       bothmodel_rmse = rep(NA,length(content_ids)),
                       flag_discrim=rep(0,length(content_ids)), #If upper vs lower students diff is low item should be reviewed
                       flag_diffupper=rep(0,length(content_ids))) # If lower than 60% correct on an item for upper quartile of students, flag

stu_perf = tapply(val_trim$CF..ansbin.,val_trim$Anon.Student.Id,function(x){mean(x)})
quants = quantile(stu_perf, prob=c(.25,.5,.75))
#Upper quartile students
upper_stu = names(stu_perf[which(stu_perf>quants[3])])
#lower quartile students
lower_stu = names(stu_perf[which(stu_perf<quants[1])])

for(i in 1:length(content_ids)){
  item_disc$upper[i] = mean(tapply(val_trim$CF..ansbin.[which(val_trim$Anon.Student.Id %in% upper_stu & val_trim$content_id == content_ids[i])],val_trim$Anon.Student.Id[which(val_trim$Anon.Student.Id %in% upper_stu & val_trim$content_id == content_ids[i])],function(x){mean(x)}))
  item_disc$lower[i] = mean(tapply(val_trim$CF..ansbin.[which(val_trim$Anon.Student.Id %in% lower_stu & val_trim$content_id == content_ids[i])],val_trim$Anon.Student.Id[which(val_trim$Anon.Student.Id %in% lower_stu & val_trim$content_id == content_ids[i])],function(x){mean(x)}))
  item_disc$discrim[i] = item_disc$upper[i]-item_disc$lower[i]
  idx=which(val_trim$content_id==content_ids[i])
  item_disc$N[i] = length(idx)
  
  if(length(idx)>0){
    item_disc$bothmodel_rmse[i] = sqrt(mean((fmod_tagboth$prediction[idx]-fmod_tagboth$newdata$CF..ansbin.[idx])^2))
    item_disc$item_perf[i] = mean(fmod_tagboth$newdata$CF..ansbin.[idx])
    item_disc$item_pred[i] = mean(fmod_tagboth$prediction[idx])
  }
  print(i)
}

#These cutoffs are common rules of thumb, not set in stone
item_disc$flag_discrim[which(item_disc$discrim<.2)] = 1
item_disc$flag_diffupper[which(item_disc$upper<.6)] = 1

write.csv(item_disc,file="item_discrim.csv")



AP_tags = unique(as.vector(tag_auto))
AP_trunc = unique(substr(AP_tags,1,7))



AP_trunc = unique(substr(AP_tags,1,7))
val_trim$ap_trunc = rep("placeholder",length(val_trim$date))
#For each AP trunc, do str detect
for(i in 1:length(AP_trunc)){
  val_trim$ap_trunc[which(str_detect(val_trim$tag_auto,pattern=AP_trunc[i])==TRUE)] = AP_trunc[i]
  print(i)
}

#Make a column in val_trim for each unique id
val_trim = as.data.frame(val_trim)
AP_tags = AP_tags[which(!is.na(AP_tags))]
val_trim[AP_tags] <- 0
#Now roll through str_detect() to find the tags in tag_auto
for(i in 1:length(AP_tags)){
  idx = which(str_detect(str_squish(val_trim$tag_auto),AP_tags[i])==TRUE)
  col_id = which(names(val_trim)==AP_tags[i])
  val_trim[idx,col_id] = 1
  print(i)
}

##Relative importance
library(fastDummies)
library(xgboost)
val_dum = cbind(val_trim$CF..ansbin.,val_trim[,43:257])
names(val_dum)[1] = "CF..ansbin."
tictoc::tic()
glm1 = glm(CF..ansbin. ~.,data=val_dum[1:10000,])
tictoc::toc()

endt = floor(.8*length(val_dum[,1]))
train_x = data.matrix(val_dum[1:endt, -c(1)])
train_y = ifelse(val_dum[1:endt,1]==1,1,0)

#define predictor and response variables in testing set
test_x = data.matrix(val_dum[(endt+1):length(val_dum[,1]), -c(1)])
test_y = ifelse(val_dum[(endt+1):length(val_dum[,1]),1]==1,1,0)

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, num_parallel_tree=1, nrounds = 100,eval_metric="auc",objective="binary:logistic")
xgb.plot.tree(model=model, tree=1)
preds = predict(model,test_x)
hist(preds[which(test_y<.5)])
hist(preds[which(test_y>=.5)])
importance = xgb.importance(model=model)
#Of tag combinations
xgb.plot.importance(importance_matrix=importance)

###Clustering