library(randomForest)
library(foreach)
library(doParallel)
library(data.table)

cl<-makeCluster(4)#Cores
registerDoParallel(cl)

train.cl<-data.table(train.cl)
setkey(train.cl,Store)
#samp<-sample(1:nrow(train.cl),nrow(train.cl)*0.3)

#first 10 stores, as a test.

subset<-subset(train.cl,Store%in%c(1:10))
models<-foreach(i=1:10,.packages="randomForest") %dopar% {
        model_rf<-randomForest(Sales~.-Store,data=subset[subset$Store==i,])
        }
