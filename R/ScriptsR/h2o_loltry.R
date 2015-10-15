library(data.table)  
library(h2o)

train <- fread("D:/Kaggle/Rossmann/TryOne/train.csv",stringsAsFactors = T)
test  <- fread("D:/Kaggle/Rossmann/TryOne/test.csv",stringsAsFactors = T)
store <- fread("D:/Kaggle/Rossmann/TryOne/store.csv",stringsAsFactors = T)
train <- train[Sales > 0,]

train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")

train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train[,day:=as.integer(format(Date, "%d"))]
train[,Store:=as.factor(as.numeric(Store))]

test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,day:=as.integer(format(Date, "%d"))]
test[,Store:=as.factor(as.numeric(Store))]

train[,logSales:=log1p(Sales)]

#Calculate if Promo2 is active
train[,Promo2D:=as.numeric(round(difftime(Date,as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),units="days")))]
train[,IntervF:=ifelse(as.character(PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))]
train[,DateMIN:=ifelse(is.element(month,c(2,5,8,11)),1,ifelse(is.element(month,c(1,4,7,10)),2,ifelse(is.element(month,c(3,6,9,12)),3,0)))]
train[,Promo2APrev:=DateMIN - IntervF]
train[Promo2APrev == 0,Promo2A:=ifelse(DateMIN != 0 && Promo2D > 0,1,0)]
train[is.na(Promo2A),Promo2A:=0]

test[,Promo2D:=as.numeric(round(difftime(Date,as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),units="days")))]
test[,IntervF:=ifelse(as.character(PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))]
test[,DateMIN:=ifelse(is.element(month,c(2,5,8,11)),1,ifelse(is.element(month,c(1,4,7,10)),2,ifelse(is.element(month,c(3,6,9,12)),3,0)))]
test[,Promo2APrev:=DateMIN - IntervF]
test[Promo2APrev == 0,Promo2A:=ifelse(DateMIN != 0 && Promo2D > 0,1,0)]
train[is.na(Promo2A),Promo2A:=0]

#Calculate if competitor is up
train[,COSD:=as.numeric(round(difftime(Date,as.POSIXct((paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)),format="%d %m %Y"),units="days")))]
train[,CompetitorA:=ifelse(COSD > -1,1,0)]
train[is.na(CompetitionDistance),CompetitorA:=0]
train[is.na(COSD),CompetitorA:=ifelse(is.na(COSD),0,0)]

test[,COSD:=as.numeric(round(difftime(Date,as.POSIXct((paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)),format="%d %m %Y"),units="days")))]
test[,CompetitorA:=ifelse(COSD > -1,1,0)]
test[is.na(CompetitionDistance),CompetitorA:=0]
test[is.na(COSD),CompetitorA:=ifelse(is.na(COSD),0,0)]

#Cleaniiiiiinnng uuuUUUuuuUUUUU
train[,Date:=NULL]
train[,Sales:=NULL]
train[,Customers:=NULL]
train[,CompetitionDistance:=NULL]
train[,CompetitionOpenSinceMonth:=NULL]
train[,CompetitionOpenSinceYear:=NULL]
train[,Promo2:=NULL]
train[,Promo2SinceWeek:=NULL]
train[,Promo2SinceYear:=NULL]
train[,PromoInterval:=NULL]
train[,Promo2D:=NULL]
train[,IntervF:=NULL]
train[,DateMIN:=NULL]
train[,Promo2APrev:=NULL]
train[,COSD:=NULL]


test[,Date:=NULL]
test[,CompetitionDistance:=NULL]
test[,CompetitionOpenSinceMonth:=NULL]
test[,CompetitionOpenSinceYear:=NULL]
test[,Promo2:=NULL]
test[,Promo2SinceWeek:=NULL]
test[,Promo2SinceYear:=NULL]
test[,PromoInterval:=NULL]
test[,Promo2D:=NULL]
test[,IntervF:=NULL]
test[,DateMIN:=NULL]
test[,Promo2APrev:=NULL]
test[,COSD:=NULL]
test[is.na(Open),Open:=0]
test[is.na(Promo2A),Promo2A:=0]


#READY!!!

#Preparing dah monstah
h2o.init(nthreads=-1,max_mem_size='12G')

trainHex<-as.h2o(train)

features<-colnames(train)[!(colnames(train) %in% c("Id","logSales"))]

rfHex <- h2o.randomForest(x=features,
                          y="logSales", 
                          ntrees = 200,
                          max_depth = 30,
                          nbins_cats = 1115,
                          training_frame=trainHex)


testHex<-as.h2o(test)


predictions<-as.data.frame(h2o.predict(rfHex,testHex))

pred <- expm1(predictions[,1])

submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write.csv(submission, "H2oFirstTry.csv",row.names=F)
