#DOK


dok = read.csv("csawesome-eks-dok-auto-exploded - csawesome-eks-dok-auto-exploded.csv")

val_trim$dok = rep(NA,length(val_trim$date))

val_trim$content_id[1]
val_trim$tag_auto[1]

which(str_detect(val_trim$tag_auto,"CON-2.E.1"))

tags = unique(unlist(str_split(val_trim$tag_auto,"_")))
tags = str_replace_all(tags," ", "")

#Get performance per tag across items e.g.,
tag_perf = rep(NA,length(tags))
dok_tag = rep(NA,length(tags))
for(i in 1:length(tags)){
  if(tags[i]==""){
    
  }else{
  idx = which(str_detect(val_trim$tag_auto,tags[i]))
  tag_perf[i] = mean(val_trim$CF..ansbin.[idx],na.rm=TRUE)
  dok_tag[i] = mean(dok$EK.DOK[which(dok$EK==tags[i])])
  }
  print(i)
}

idx=which(dok_tag!=-1)
plot(dok_tag[idx],tag_perf[idx],pch=16,xlab="Average DOK",ylab="Avg Performance on item with tag")
cor.test(dok_tag[idx],tag_perf[idx])
