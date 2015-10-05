library(dplyr)
library(outliers)


##Functions!
regularPromoThisMonth<-function(monthSale,interval){
  returnVal<-NA
  if(nchar(interval)>0){
    months<-unlist(strsplit(interval,","))
    numeric_months<-match(months,month.abb)
    returnVal<-monthSale %in% numeric_months
  }else{
    returnVal<- F
  }
  return(returnVal)
}




store <- read.csv("~/workspaces/Rprojects/KgShop/store.csv",stringsAsFactors=F)
#test <- read.csv("~/workspaces/Rprojects/KgShop/test.csv",stringsAsFactors=F)
train <- read.csv("~/workspaces/Rprojects/KgShop/train.csv",stringsAsFactors=F)


#Store changes:

#All "promo" features are basically the same description, we'll simplify that in a bit.



#with the same colums as test
train.cl<-train[,c("Store","DayOfWeek","Date","Open","Promo","StateHoliday","SchoolHoliday","Sales")]
train.cl<-inner_join(store,train.cl,by="Store")
rm(train)
#Challenge started: Wed 30 Sep 2015
startDate<-as.POSIXct("30-09-2015",format="%d-%m-%Y")
train.cl$Date<-as.POSIXct(train.cl$Date,format="%Y-%m-%d")

##I'm using date and factor back and forth because dplyr and rowwise_df don't work very well with each other.


#New Field, competition open up to the "sale" date info.
train.cl<-mutate(train.cl,
                 competitionOpenSince=round(difftime(Date,
                                                     as.POSIXct(paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear),format="%d %m %Y"),
                                                     units="weeks"))
)    

## Is the promo2 active on this day?
train.cl<-mutate(train.cl,monthOfSale=as.numeric(format(Date,"%m"))) ##to get the month date
train.cl<-data.frame(train.cl %>%
  rowwise() %>%
  mutate(   Promo2InProgress=regularPromoThisMonth(monthOfSale,as.character(PromoInterval))
))
# For how long has been this store using promo2
# using monday as "day since start" because reasons.
train.cl<-mutate(train.cl,
                 Promo2Since=round(difftime(Date,
                                            as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),
                                            units="weeks"))
                 )

#Cleaning!
train.cl<-train.cl[,c("Store","StoreType","Assortment","CompetitionDistance","competitionOpenSince",
                      "DayOfWeek","monthOfSale","Open","Promo","Promo2InProgress","StateHoliday","SchoolHoliday","Sales")]
train.cl$competitionOpenSince<-as.numeric(train.cl$competitionOpenSince) #As numeric. Value in weeks
train.cl$StoreType<-factor(train.cl$StoreType)
train.cl$Assortment<-factor(train.cl$Assortment)
train.cl$Promo<-factor(train.cl$Promo)
train.cl$StateHoliday<-factor(train.cl$StateHoliday)
train.cl$SchoolHoliday<-factor(train.cl$SchoolHoliday)
train.cl$DayOfWeek<-factor(train.cl$DayOfWeek,ordered = T)
train.cl$monthOfSale<-factor(train.cl$monthOfSale,ordered = T)
train.cl$Open<-factor(train.cl$Open)

train.cl$competitionOpenSince[train.cl$competitionOpenSince<0]<-0



###Outlier time!


comp.distance<-boxplot.stats(train.cl$CompetitionDistance)$stats[5]
outliers<-outlier(train.cl$CompetitionDistance,logical = T)
train.cl$CompetitionDistance[outliers]<-comp.distance*3.8

# Outliers package marks only one here as outlier, and just barely. Not worth it.
# comp.age<-boxplot.stats(train.cl$competitionOpenSince)$stats[5]
# outliers<-outlier(train.cl$competitionOpenSince,logical = T)
# train.cl$competitionOpenSince[outliers]<-round(comp.age*3.8)

#NA values. Time to deal with it.
#-999 for now. Cos reasons
train.cl$CompetitionDistance[is.na(train.cl$CompetitionDistance)]<- -999
train.cl$competitionOpenSince[is.na(train.cl$competitionOpenSince)]<- -999
