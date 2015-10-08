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

#Transform store DF
df$Date<-as.POSIXct(df$Date,format="%Y-%m-%d")
df<-mutate(df,Promo2D=round(difftime(Date,as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),units="days")))
df<-mutate(df,COSD=round(difftime(Date,as.POSIXct((paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)),format="%d %m %Y"),units="days")))
df<-mutate(df,DateM=month.abb[as.POSIXlt(Date,format="%Y-%m-%d")$mon + 1])

df$IntTMP[!is.na(df$PromoInterval)] <- ifelse(as.character(df$PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(df$PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(df$PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))
df$DateMI <- ifelse(is.element(df$DateM,c("Feb","May","Aug","Nov")),1,ifelse(is.element(df$DateM,c("Jan","Apr","Jul","Oct")),2,ifelse(is.element(df$DateM,c("Mar","Jun","Sept","Dec")),3,0)))
df$P2A <- 0
df$P2A[!is.na(df$Promo2D)] <- ifelse(df$DateMI[!is.na(df$Promo2D)] == df$IntTMP[!is.na(df$Promo2D)],1,0)


df<-df[,c("Store","DayOfWeek","Sales","Open","Promo","StateHoliday","SchoolHoliday","CompetitionDistance","COSD","P2A","Promo2D","DateM")]

df$COSD<-as.numeric(df$COSD)
df$COSD[is.na(df$COSD)] <- -999
df$CompetitionDistance[is.na(df$CompetitionDistance)] <- 75860

df$Sales<-as.numeric(df$Sales)
df$P2A <- factor(df$P2A)
df$StateHoliday <- factor(df$StateHoliday)
df$Open <- factor(df$Open)
df$Promo <- factor(df$Promo)
df$DayOfWeek <- factor(df$DayOfWeek)
df$DateM<- factor(df$DateM,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df$Promo2D<-as.numeric(df$Promo2D)
df$Promo2D[is.na(df$Promo2D)] <- -888
df$SchoolHoliday<-factor(df$SchoolHoliday)

df <- filter(df, Open == 1)

#List of DFs / Store(factor)
dflist <- split(df,f = df$Store)

foreach(i=1:length(dflist)) %do% {
  dflist[[i]]$Store<-factor(dflist[[i]]$Store)
}

#Training the model ( randomForest)
cl <- makeCluster(4)
registerDoParallel(cl)
selva <- vector("list",length(dflist))

for (i in 1:length(selva)){
  selva[[i]]<- tuneRF(dflist[[i]][,-3],dflist[[i]]$Sales,stepFactor = 1.5,ntreeTry = 180,doBest = T)
}

#Adapt test DF
dftest <- inner_join(test,store,by="Store")

dftest$Date<-as.POSIXct(dftest$Date,format="%Y-%m-%d")
dftest<-mutate(dftest,Promo2D=round(difftime(Date,as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),units="days")))
dftest<-mutate(dftest,COSD=round(difftime(Date,as.POSIXct((paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)),format="%d %m %Y"),units="days")))
dftest<-mutate(dftest,DateM=month.abb[as.POSIXlt(Date,format="%Y-%m-%d")$mon + 1])

dftest$IntTMP[!is.na(dftest$PromoInterval)] <- ifelse(as.character(dftest$PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(dftest$PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(dftest$PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))
dftest$DateMI <- ifelse(is.element(dftest$DateM,c("Feb","May","Aug","Nov")),1,ifelse(is.element(dftest$DateM,c("Jan","Apr","Jul","Oct")),2,ifelse(is.element(dftest$DateM,c("Mar","Jun","Sept","Dec")),3,0)))
dftest$P2A <- 0
dftest$P2A[!is.na(dftest$Promo2D)] <- ifelse(dftest$DateMI[!is.na(dftest$Promo2D)] == dftest$IntTMP[!is.na(dftest$Promo2D)],1,0)
dftest$Sales <- 0

dftest<-dftest[,c("Store","DayOfWeek","Sales","Open","Promo","StateHoliday","SchoolHoliday","CompetitionDistance","COSD","P2A","Promo2D","DateM")]

#NAs
dftest$COSD<-as.numeric(dftest$COSD)
dftest$COSD[is.na(dftest$COSD)] <- 0
dftest$Open[is.na(dftest$Open)] <- 0
dftest$CompetitionDistance[is.na(dftest$CompetitionDistance)] <- 75860

dftest$Sales<-as.numeric(dftest$Sales)
dftest$P2A <- factor(dftest$P2A)
dftest$StateHoliday <- factor(dftest$StateHoliday,levels=c("0","a","b","c"))
dftest$Open <- factor(dftest$Open)
dftest$Promo <- factor(dftest$Promo)
dftest$DayOfWeek <- factor(dftest$DayOfWeek)
dftest$DateM<- factor(dftest$DateM,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
dftest$Promo2D<-as.numeric(dftest$Promo2D)
dftest$Promo2D[is.na(dftest$Promo2D)] <- 54
dftest$SchoolHoliday<-factor(dftest$SchoolHoliday)

#Create a list of predictions from adapted test DF

for (i in 1:nrow(dftest)){
  op <- as.integer(dftest[i,]$Open)
  if (op == 2){
    n <- as.integer(dftest[i,]$Store)
    dftest[i,]$Sales=predict(selva[[n]],dftest[i,])
  }
}


#Create a submit DF
submit <- data.frame(Id = test$Id, Sales = dftest$Sales)

#Save into csv
write.csv(submit, file = "RossmanTryFive.csv", row.names = FALSE)
