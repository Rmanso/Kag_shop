library(doParallel)
library(randomForest)
library(foreach)

# Set working directory
#setwd("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann")#


## Import data sets into DFs
#train <- read.csv("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann/train.csv")
#store <- read.csv("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann/store.csv")
#test  <- read.csv("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann/test.csv" )

train <- read.csv("D:/Kaggle/Rossmann/TryOne/train.csv")
store <- read.csv("D:/Kaggle/Rossmann/TryOne/store.csv")
test  <- read.csv("D:/Kaggle/Rossmann/TryOne/test.csv" )


#Transform store DF
# Competition Open Since Date [COSD[( year + month)
# Promo2Date[P2D] ( year + month )
store$Prom2D <- as.Date(paste(store$Promo2SinceYear,store$Promo2SinceWeek,1),"%Y %U %u")
store$COSD <- as.Date(paste(store$CompetitionOpenSinceYear,store$CompetitionOpenSinceMonth,1),"%Y %m %d")
store$STA <- paste(store$StoreType,store$Assortment,sep="")
store$CompetitionOpenSinceMonth <- NULL
store$CompetitionOpenSinceYear <- NULL
store$Promo2SinceWeek <- NULL
store$Promo2SinceYear <- NULL
store$Promo2 <- NULL
store$StoreType <- NULL
store$Assortment <- NULL

#Transform train DF
train$Customers <- NULL

#Join both modified DFs
dfPrev <- merge(train,store,by.x ="Store")

#Transform new DF
#Competitor Date distance
dfPrev$CD <- difftime(as.Date(dfPrev$Date,format = "%Y-%m-%d"),as.Date(dfPrev$COSD,format="%Y-%m%d"),units = "days")
#Promo 2 Date distance
dfPrev$P2D <- difftime(as.Date(dfPrev$Date,format = "%Y-%m-%d"),as.Date(dfPrev$Prom2D,format="%Y-%m%d"),units = "days")

dfPrev$DateM <- month.abb[as.POSIXlt(dfPrev$Date,format="%Y-%m-%d")$mon + 1]
dfPrev$P2A <- 0

#Interval -> 3 categories
dfPrev$IntTMP[!is.na(dfPrev$PromoInterval)] <- ifelse(as.character(dfPrev$PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(dfPrev$PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(dfPrev$PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))
#Date Month Interval
dfPrev$DateMI <- ifelse(is.element(dfPrev$DateM,c("Feb","May","Aug","Nov")),1,ifelse(is.element(dfPrev$DateM,c("Jan","Apr","Jul","Oct")),2,ifelse(is.element(dfPrev$DateM,c("Mar","Jun","Sept","Dec")),3,0)))
#Promo 2 Active
dfPrev$P2A[dfPrev$IntTMP != 0] <- ifelse(dfPrev$DateMI[dfPrev$IntTMP != 0] == dfPrev$IntTMP[dfPrev$IntTMP != 0],1,0)

dfPrev$Store <- as.factor(dfPrev$Store)
dfPrev$P2A <- as.factor(dfPrev$P2A)
dfPrev$StateHoliday <- as.factor(dfPrev$StateHoliday)
dfPrev$SchoolHoliday <- as.factor(dfPrev$SchoolHoliday)
dfPrev$Open <- as.factor(dfPrev$Open)
dfPrev$Promo <- as.factor(dfPrev$Promo)
dfPrev$P2A <- as.factor(dfPrev$P2A)
dfPrev$DayOfWeek <- as.factor(dfPrev$DayOfWeek)
dfPrev$STA <- as.factor(dfPrev$STA)
dfPrev$CD <- as.numeric(dfPrev$CD)
dfPrev$P2D <- as.numeric(dfPrev$P2D)
dfPrev$Date <- NULL
dfPrev$IntTMP <- NULL
dfPrev$DateM <- NULL
dfPrev$DateMI <- NULL
dfPrev$PromoInterval <- NULL
dfPrev$COSD <- NULL
dfPrev$Prom2D <- NULL

dfPrev$Store <- as.numeric(dfPrev$Store)
#NAs
dfPrev$CompetitionDistance[is.na(dfPrev$CompetitionDistance)] <- -1
dfPrev$CD[is.na(dfPrev$CD)] <- -900
dfPrev$P2D[is.na(dfPrev$P2D)] <- -900
dfPrev$STA<-factor(dfPrev$STA,levels=c("aa","ab","ac","ad","ba","bb","bc","bd","ca","cb","cc","cd","da","db","dc","dd"),ordered = F)
dfPrev$Sales<-as.numeric(dfPrev$Sales)
#List of DFs / Store(factor)
dflist <- split(dfPrev,f = dfPrev$Store)

#Training the model ( randomForest)
cl <- makeCluster(4)
registerDoParallel(cl)

daForest <- foreach(i=1:length(dflist), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
  randomForest(dflist[[i]][,-3],dflist[[i]]$Sales , ntree=180) 
}

#Adapt test DF

dftest <- merge(test,store,by.x ="Store")

#Transform new DF
#Competitor Date distance
dftest$CD <- difftime(as.Date(dftest$Date,format = "%Y-%m-%d"),as.Date(dftest$COSD,format="%Y-%m%d"),units = "days")
#Promo 2 Date distance
dftest$P2D <- difftime(as.Date(dftest$Date,format = "%Y-%m-%d"),as.Date(dftest$Prom2D,format="%Y-%m%d"),units = "days")

dftest$DateM <- month.abb[as.POSIXlt(dftest$Date,format="%Y-%m-%d")$mon + 1]
dftest$P2A <- 0

#Interval -> 3 categories
dftest$IntTMP[!is.na(dftest$PromoInterval)] <- ifelse(as.character(dftest$PromoInterval) == "Feb,May,Aug,Nov",1,ifelse(as.character(dftest$PromoInterval) == "Jan,Apr,Jul,Oct",2,ifelse(as.character(dftest$PromoInterval) == "Mar,Jun,Sept,Dec",2,0)))
#Date Month Interval
dftest$DateMI <- ifelse(is.element(dftest$DateM,c("Feb","May","Aug","Nov")),1,ifelse(is.element(dftest$DateM,c("Jan","Apr","Jul","Oct")),2,ifelse(is.element(dftest$DateM,c("Mar","Jun","Sept","Dec")),3,0)))
#Promo 2 Active
dftest$P2A[dftest$IntTMP != 0] <- ifelse(dftest$DateMI[dftest$IntTMP != 0] == dftest$IntTMP[dftest$IntTMP != 0],1,0)

dftest$Store <- as.factor(dftest$Store)
dftest$P2A <- as.factor(dftest$P2A)
dftest$StateHoliday <- as.factor(dftest$StateHoliday)
dftest$SchoolHoliday <- as.factor(dftest$SchoolHoliday)
dftest$Open <- as.factor(dftest$Open)
dftest$Promo <- as.factor(dftest$Promo)
dftest$P2A <- as.factor(dftest$P2A)
dftest$DayOfWeek <- as.factor(dftest$DayOfWeek)
dftest$STA <- as.factor(dftest$STA)
dftest$CD <- as.numeric(dftest$CD)
dftest$P2D <- as.numeric(dftest$P2D)
dftest$Date <- NULL
dftest$IntTMP <- NULL
dftest$DateM <- NULL
dftest$DateMI <- NULL
dftest$PromoInterval <- NULL
dftest$COSD <- NULL
dftest$Prom2D <- NULL

dftest$Store <- as.numeric(dftest$Store)

dftest$Id <- NULL
dftest$Sales <- 0

#NAs
dftest$CompetitionDistance[is.na(dftest$CompetitionDistance)] <- -1
dftest$CD[is.na(dftest$CD)] <- -900
dftest$P2D[is.na(dftest$P2D)] <- -900
dftest$Open[is.na(dftest$Open)] <- 0

#Extras
dftest$StateHoliday<-factor(dftest$StateHoliday,levels=c("0","a","b","c"))
dftest$DayOfWeek<-factor(dftest$DayOfWeek,levels=c("1","2","3","4","5","6","7"),ordered = F)
dftest$STA<-factor(dftest$STA,levels=c("aa","ab","ac","ad","ba","bb","bc","bd","ca","cb","cc","cd","da","db","dc","dd"))


#Create a list of predictions from adapted test DF
Prediction <- predict(daForest,dftest)


#Create a submit DF
submit <- data.frame(Id = test$Id, Sales = Prediction)

#Save into csv
write.csv(submit, file = "RossmanTryOne.csv", row.names = FALSE)
