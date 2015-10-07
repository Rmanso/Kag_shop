library(h2o)
#library(mlbench)

#init h2o
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                    Xmx = '5g')

#Remove ordered factors
train.cl$DayOfWeek<-factor(train.cl$DayOfWeek,ordered = F)
train.cl$monthOfSale<-factor(train.cl$monthOfSale,ordered=F)

dat_h2o <- as.h2o(conn=localH2O, object=train.cl[,c(1,2,3,4,5,6,7,8,9,10,13)], destination_frame= 'train_df')
#it works!

#70/30

samp<-sample(nrow(train.cl),nrow(train.cl)*0.7)
not_samp<-setdiff(1:nrow(train.cl),samp)
y_all <- as.matrix(dat_h2o[,13])
y_train <- as.factor(y_all[samp])
y_test <- as.factor(y_all[-samp])

model <- 
  h2o.deeplearning(x = names(dat_h2o)[1:12],  # column numbers for predictors
                   y = "Sales",   # column number for label
                   training_frame = dat_h2o[samp,], # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(20,20,20), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

yhat_train <- h2o.predict(model, dat_h2o[samp, ])$predict
yhat_train <- as.factor(as.matrix(yhat_train))
yhat_test <- h2o.predict(model, dat_h2o[not_samp, ])$predict
yhat_test <- as.factor(as.matrix(yhat_test))

confusionMatrix(yhat_train, train.cl$Sales)
confusionMatrix(yhat_test, y_test)


model_rf_powah<-h2o.randomForest(x=names(dat_h2o)[1:9],
                           y = "Sales",
                           training_frame = dat_h2o[samp,],
                           validation_frame = dat_h2o[not_samp,],
                           ntrees = 300,
                           max_depth = 60,
                           model_id= "DakotaJonson")

test_h2o <- as.h2o(conn=localH2O, object=test.cl[,c(1,2,3,4,5,6,7,8,9,10)], destination_frame= 'test_df')



predictions<-h2o.predict(model_rf_powah,newdata=test_h2o)
result<-data.frame("Id"=test.cl$Id,"Sales"=as.matrix(predictions))
names(result)[2]<-"Sales"
write.csv(result,file="result_rf2.csv",row.names=F,col.names=T)
h2o.shutdown()
Y
