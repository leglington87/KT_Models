compl = c("tag_manual","tag_auto","tag_manual","tag_auto")
connl = c("+","+","+","+")
autol <- c(60,60,0,0)
featl = c("intercept","intercept","logafm$","logafm$")
modelob <<- LKT(data = val_trim,components = compl,features = featl,connectors = connl,autoKC = autol,
                cv=FALSE,verbose = TRUE)
modelob$cv_res

#normal model for comparison

compl = c("tag_manual","tag_auto")
connl = c("+","+")
autol <- c(0,0)
featl = c("intercept","intercept")
modelob2 <<- LKT(data = val_trim,components = compl,features = featl,connectors = connl,
                 cv=TRUE,verbose = TRUE)
modelob2$cv_res



#How much does autoKC agree? Does it cluster tag_manual and tag_auto similarly?
compl = c("tag_manual","tag_manual")
connl = c("+","+","+","+")
autol <- c(0,0)
featl = c("intercept","logafm$")
modelob0 <<- LKT(data = val_trim,components = compl,features = featl,connectors = connl,autoKC = autol,
                 cv=TRUE,verbose = TRUE)

compl = c("tag_manual","tag_manual")
connl = c("+","+","+","+")
autol <- c(60,60)
featl = c("intercept","logafm$")
modelob1 <<- LKT(data = val_trim,components = compl,features = featl,connectors = connl,autoKC = autol,
                 cv=TRUE,verbose = TRUE)


View(modelob1$newdata[which(modelob1$newdata$AC1==5),c("AC1","tag_manual")])

idx5_1=which(modelob1$newdata$AC1==5)

compl = c("tag_auto","tag_auto")
connl = c("+","+","+","+")
autol <- c(60,60)
featl = c("intercept","logafm$")
modelob2 <<- LKT(data = val_trim,components = compl,features = featl,connectors = connl,autoKC = autol,
                 cv=TRUE,verbose = TRUE)

View(modelob2$newdata[which(modelob2$newdata$AC1==5),c("AC1","tag_auto")])
idx5_2=which(modelob2$newdata$AC1==5)

length(idx5_1)
View(modelob1$newdata[idx5_1,"chapter"])
length(idx5_2)

#What is cluster for auto when it's above "Unit5-Writing-Classes"
unique(modelob1$newdata$chapter)
manual_Clust = sort(as.numeric(unique(modelob1$newdata$AC1[which(modelob1$newdata$chapter=="Unit1-Getting-Started")])))
auto_Clust = as.numeric(sort(unique(modelob2$newdata$AC1[which(modelob2$newdata$chapter=="Unit1-Getting-Started")])))
length(which(manual_Clust %in% auto_Clust))/length(manual_Clust)
View(modelob1$newdata[idx5_2,"chapter"])

tag_man_rmse = tapply(modelob1$prediction-modelob1$newdata$CF..ansbin.,modelob1$newdata$chapter,function(x){sqrt(mean(x^2))})
tag_auto_rmse = tapply(modelob2$prediction-modelob2$newdata$CF..ansbin.,modelob2$newdata$chapter,function(x){sqrt(mean(x^2))})

tag_auto_rmse-tag_man_rmse

# two tags


compl = c("tag_manual","tag_manual","tag_manual","tag_manual")
connl = c("+","+","+","+")
autol <- c(0,0,60,60)
featl = c("intercept","logafm$","intercept","logafm$")
modelob4 <<- LKT(data = val_trim,components = compl,features = featl,connectors = connl,autoKC = autol,
                 cv=TRUE,verbose = TRUE)
modelob4$cv_res

