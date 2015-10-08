library(doParallel)
library(randomForest)
library(foreach)
library(dplyr)

# Set working directory
setwd("D:/Kaggle/Rossmann/TryOne")

# Import data sets into DFs
train <- read.csv("D:/Kaggle/Rossmann/TryOne/train.csv")
store <- read.csv("D:/Kaggle/Rossmann/TryOne/store.csv")
test  <- read.csv("D:/Kaggle/Rossmann/TryOne/test.csv" )

#Merge DFs
df<-inner_join(train,store,by="Store")
dftest <- inner_join(test,store,by="Store")

#Transform DFs
df <- filter(df, Open == 1)

df<-mutate(df,Date=as.Date(Date))
dftest<-mutate(dftest,Date=as.Date(Date))

df<-mutate(df,day=as.integer(format(Date, "%d")))
df<-mutate(df,month=as.integer(format(Date, "%m")))
df<-mutate(df,year=as.integer(format(Date, "%y")))
dftest<-mutate(dftest,day=as.integer(format(Date, "%d")))
dftest<-mutate(dftest,month=as.integer(format(Date, "%m")))
dftest<-mutate(dftest,year=as.integer(format(Date, "%y")))

#df<-mutate(df,Store=as.factor(as.numeric(Store)))
dftest<-mutate(dftest,Store=as.factor(as.numeric(Store)))

df<-mutate(df,Promo2D=round(difftime(Date,as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),units="days")))
df<-mutate(df,COSD=round(difftime(Date,as.POSIXct((paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)),format="%d %m %Y"),units="days")))
df$IntTMP[!is.na(df$PromoInterval)] <- ifelse(as.character(df$PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(df$PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(df$PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))
df$DateMI <- ifelse(is.element(df$month,c("Feb","May","Aug","Nov")),1,ifelse(is.element(df$month,c("Jan","Apr","Jul","Oct")),2,ifelse(is.element(df$month,c("Mar","Jun","Sept","Dec")),3,0)))
df$P2A <- 0
df$P2A[!is.na(df$Promo2D)] <- ifelse(df$DateMI[!is.na(df$Promo2D)] == df$IntTMP[!is.na(df$Promo2D)],1,0)
df<-mutate(df,lSales=log1p(Sales))

dftest<-mutate(dftest,Promo2D=round(difftime(Date,as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),units="days")))
dftest<-mutate(dftest,COSD=round(difftime(Date,as.POSIXct((paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)),format="%d %m %Y"),units="days")))
dftest<-mutate(dftest,DateM=month.abb[as.POSIXlt(Date,format="%Y-%m-%d")$mon + 1])

dftest$IntTMP[!is.na(dftest$PromoInterval)] <- ifelse(as.character(dftest$PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(dftest$PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(dftest$PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))
dftest$DateMI <- ifelse(is.element(dftest$month,c("Feb","May","Aug","Nov")),1,ifelse(is.element(dftest$month,c("Jan","Apr","Jul","Oct")),2,ifelse(is.element(dftest$month,c("Mar","Jun","Sept","Dec")),3,0)))
dftest$P2A <- 0
dftest$P2A[!is.na(dftest$Promo2D)] <- ifelse(dftest$DateMI[!is.na(dftest$Promo2D)] == dftest$IntTMP[!is.na(dftest$Promo2D)],1,0)
dftest$Sales <- 0

df$COSD<-as.numeric(df$COSD)
df$COSD[is.na(df$COSD)] <- -1
df$CompetitionDistance[is.na(df$CompetitionDistance)] <- -1

df$Sales<-as.numeric(df$Sales)
df$P2A <- factor(df$P2A)
df$StateHoliday <- factor(df$StateHoliday)
df$Open <- factor(df$Open)
df$Promo <- factor(df$Promo)
df$DayOfWeek <- factor(df$DayOfWeek)
df$DateM<- factor(df$DateM,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df$Promo2D<-as.numeric(df$Promo2D)
df$Promo2D[df$Promo2D < 0] <- -1
df$Promo2D[is.na(df$Promo2D)] <- -1
df$SchoolHoliday<-factor(df$SchoolHoliday)

dftest$COSD<-as.numeric(dftest$COSD)
dftest$COSD[is.na(dftest$COSD)] <- 0
dftest$Open[is.na(dftest$Open)] <- 0
dftest$CompetitionDistance[is.na(dftest$CompetitionDistance)] <- -1

dftest$Sales<-as.numeric(dftest$Sales)
dftest$lSales<- 0
dftest$P2A <- factor(dftest$P2A)
dftest$StateHoliday <- factor(dftest$StateHoliday,levels=c("0","a","b","c"))
dftest$Open <- factor(dftest$Open)
dftest$Promo <- factor(dftest$Promo)
dftest$DayOfWeek <- factor(dftest$DayOfWeek)
dftest$Promo2D<-as.numeric(dftest$Promo2D)
dftest$Promo2D[dftest$Promo2D < 0] <- -1
dftest$Promo2D[is.na(dftest$Promo2D)] <- -1
dftest$SchoolHoliday<-factor(dftest$SchoolHoliday)


df<-df[,c("Store","DayOfWeek","lSales","Open","Promo","StateHoliday","SchoolHoliday","CompetitionDistance","COSD","P2A","Promo2D","day","month","year")]
dftest<-dftest[,c("Store","DayOfWeek","lSales","Open","Promo","StateHoliday","SchoolHoliday","CompetitionDistance","COSD","P2A","Promo2D","day","month","year")]

dflist <- split(df,f = df$Store)

for (i in 1:length(dflist)){
  dflist[[i]]$Store<-factor(dflist[[i]]$Store)
}

#Training the model ( randomForest)

cl <- makeCluster(4)
registerDoParallel(cl)
selva <- vector("list",length(dflist))

for (i in 1:length(selva)){
  selva[[i]]<- tuneRF(dflist[[i]][,-3],dflist[[i]]$lSales,stepFactor = 1.5,ntreeTry = 180,doBest = T)
}

dftest$Id <- test$Id
dflistT <- split(dftest,f = dftest$Id)

#Create a list of predictions from adapted test DF
for (i in 1:length(dflistT)){
  dflistT[[i]]$Store<-factor(dflistT[[i]]$Store)
  dflistT[[i]]$Id <- NULL
}

dftest[i,]$Sales <- 0
pred <- vector("list", length(dflistT))
for (i in 1:length(dflistT)){
  op <- as.integer(dflistT[[i]]$Open)
  cat("i: ",i)
  if (op == 2){
    n <- as.integer(levels(dflistT[[i]]$Store))
    cat("n: ",n)
    pred[[i]]<-as.numeric(expm1(predict(selva[[n]],dflistT[[i]])))
  }else{
    pred[[i]] <- 0
  }
}


test$Sales <- pred

#Create a submit DF
submit <- data.frame(Id = test$Id, Sales = pred)

#Save into csv
write.csv(submit, file = "RossmanTrySix.csv", row.names = FALSE)
