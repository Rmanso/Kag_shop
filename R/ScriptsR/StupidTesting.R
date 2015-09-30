library(dplyr)
library(randomForest)

#load dataframes

df.test<-inner_join(store[,1:8],test[,c(2,8)],by="Store")
df.test<-df[complete.cases(df),]


prediction<-predict(model,df.test)
