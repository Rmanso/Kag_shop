library(h2o)

#another try
#this time, closed shops are out, being 0 sales by default.

#remove closed from train

train.noclosed<-train.cl[train.cl$Open==1,-8]

#store 622 is givving me trouble, so I'll cheat with it.

test.noclosed<-test.cl[test.cl$Open==1 &!is.na(test.cl$Open),] #c(1,9)
test.closed<-test.cl[test.cl$Open==0&!is.na(test.cl$Open),]
test.na<-test.cl[is.na(test.cl$Open),]

###I'll use Promo as "open"/"closed" value because



#init h2o
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                    Xmx = '5g')

dat_h2o <- as.h2o(conn=localH2O, object=train.noclosed, destination_frame= 'train_df')
test_h2o <- as.h2o(conn=localH2O, object=test.noclosed, destination_frame= 'test_df')
test_na_h2o<-as.h2o(conn=localH2O, object=test.na[test.na$Promo==1,], destination_frame= 'test_df_na')


samp<-sample(nrow(train.noclosed),nrow(train.noclosed)*0.7)
not_samp<-setdiff(1:nrow(train.noclosed),samp)


model_rf_Margot<-h2o.randomForest(x=names(dat_h2o)[1:11],
                                 y = "Sales",
                                 training_frame = dat_h2o[samp,],
                                 validation_frame = dat_h2o[not_samp,],
                                 ntrees = 300,
                                 max_depth = 60,
                                 model_id= "MargotRobbie")

#h2o.saveModel(model_rf_Margot,dir=".",name="Margot")

predictions<-h2o.predict(model_rf_Margot,newdata=test_h2o)
predictions_na<-h2o.predict(model_rf_Margot,newdata=test_na_h2o)
#result<-data.frame("Id"=test.noclosed$Id,"Sales"=as.matrix(predictions))



result_ml<-data.frame(test.noclosed$Id,as.matrix(predictions)) #ACTUAL prediction and ML stuff
result_nosales<-data.frame(test.closed$Id,0)#Those with 0 sales
result_na_sales<-data.frame(test.na[test.na$Promo==1,"Id"],as.matrix(predictions_na))#Those with NA in open but 1 in promo
result_na_no_sales<-data.frame(test.na[test.na$Promo==0,"Id"],0) 

names(result_ml)<-c("Id","Sales")
names(result_nosales)<-c("Id","Sales")
names(result_na_sales)<-c("Id","Sales")
names(result_na_no_sales)<-c("Id","Sales")


result<-rbind(result_ml,result_nosales,result_na_sales,result_na_no_sales)



write.csv(result,file="result_Margot.csv",row.names=F)


h2o.shutdown()
Y
