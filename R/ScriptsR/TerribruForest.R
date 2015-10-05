library(doMC)
library(randomForest)
library(foreach)

# Set working directory
setwd("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann")

# Import data sets into DFs
train <- read.csv("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann/train.csv")
store <- read.csv("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann/store.csv")
test  <- read.csv("C:/Users/usuario/Desktop/Formacion/Kaggle/Rossmann/test.csv" )

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
dfPrev$CD[is.na(dfPrev$CD)] <- -9999
dfPrev$P2D[is.na(dfPrev$P2D)] <- -9999

#List of DFs / Store(factor)
dflist <- split(dfPrev,f = dfPrev$Store)

#Training the model ( randomForest)
doMC(4)
terribruForest <- foreach(i:1115, .combine = combine) %dopar% {
	rf <- randomForest(Sales ~ .,data = dflist[[i]], do.trace = T, ntree = 200)
}
