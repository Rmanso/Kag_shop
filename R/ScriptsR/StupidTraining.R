library(dplyr)
library(randomForest)

#load dataframes

#too big for now, subset with only training stores
#samp<-sample(1:nrow(store),nrow(store)*0.6)


both<-intersect(unique(test[,2]),unique(train[,1]))
#now the same as test cases
shops<-inner_join(store[,1:8],data.frame("Store"=both))
df<-inner_join(shops,train[,c(1,9,4)],by="Store")
nrow(df)
df<-df[complete.cases(df),]

df$SchoolHoliday<-factor(df$SchoolHoliday)
df$CompetitionOpenSinceMonth<-factor(df$CompetitionOpenSinceMonth,ordered=T)
df$CompetitionOpenSinceYear<-2015-df$CompetitionOpenSinceYear

samp<-sample(1:nrow(df),nrow(df)*0.6)

model<-randomForest(Sales~.-Store,data=df,do.trace=T,ntree=320)
