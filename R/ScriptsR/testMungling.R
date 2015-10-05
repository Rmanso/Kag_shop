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


store <- read.csv("store.csv",stringsAsFactors=F)
test <- read.csv("test.csv",stringsAsFactors=F)

test.cl<-inner_join(store,test,by="Store")
rm(test)
#Challenge started: Wed 30 Sep 2015

test.cl$Date<-as.POSIXct(test.cl$Date,format="%Y-%m-%d")

##I'm using date and factor back and forth because dplyr and rowwise_df don't work very well with each other.


#New Field, competition open up to the "sale" date info.
test.cl<-mutate(test.cl,
                 competitionOpenSince=round(difftime(Date,
                                                     as.POSIXct(paste(1,CompetitionOpenSinceMonth,CompetitionOpenSinceYear),format="%d %m %Y"),
                                                     units="weeks"))
)    

## Is the promo2 active on this day?
test.cl<-mutate(test.cl,monthOfSale=as.numeric(format(Date,"%m"))) ##to get the month date
test.cl<-data.frame(test.cl %>%
                       rowwise() %>%
                       mutate(   Promo2InProgress=regularPromoThisMonth(monthOfSale,as.character(PromoInterval))
                       ))
# For how long has been this store using promo2
# using monday as "day since start" because reasons.
test.cl<-mutate(test.cl,
                 Promo2Since=round(difftime(Date,
                                            as.POSIXct((paste(1,Promo2SinceWeek,Promo2SinceYear)),format="%w %W %Y"),
                                            units="weeks"))
)

#Cleaning!
test.cl<-test.cl[,c("Id","Store","StoreType","Assortment","CompetitionDistance","competitionOpenSince",
                      "DayOfWeek","monthOfSale","Open","Promo","Promo2InProgress","StateHoliday","SchoolHoliday")]
test.cl$competitionOpenSince<-as.numeric(test.cl$competitionOpenSince) #As numeric. Value in weeks
test.cl$StoreType<-factor(test.cl$StoreType)
test.cl$Assortment<-factor(test.cl$Assortment)
test.cl$Promo<-factor(test.cl$Promo)
test.cl$StateHoliday<-factor(test.cl$StateHoliday,levels=c("0","a","b","c"))
test.cl$SchoolHoliday<-factor(test.cl$SchoolHoliday)
test.cl$DayOfWeek<-factor(test.cl$DayOfWeek,levels=c("1","2","3","4","5","6","7"),ordered=T)
test.cl$monthOfSale<-factor(test.cl$monthOfSale,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"),ordered = T)
test.cl$Open<-factor(test.cl$Open)

test.cl$competitionOpenSince[test.cl$competitionOpenSince<0]<-0



###Outlier time!


comp.distance<-boxplot.stats(test.cl$CompetitionDistance)$stats[5]
outliers<-outlier(test.cl$CompetitionDistance,logical = T)
test.cl$CompetitionDistance[outliers]<-comp.distance*3.8

# Outliers package marks only one here as outlier, and just barely. Not worth it.
# comp.age<-boxplot.stats(test.cl$competitionOpenSince)$stats[5]
# outliers<-outlier(test.cl$competitionOpenSince,logical = T)
# test.cl$competitionOpenSince[outliers]<-round(comp.age*3.8)

#NA values. Time to deal with it.
#-999 for now. Cos reasons
test.cl$CompetitionDistance[is.na(test.cl$CompetitionDistance)]<- -999
test.cl$competitionOpenSince[is.na(test.cl$competitionOpenSince)]<- -999
