library(data.table)
library(lubridate)
library(Smisc)
library(timeDate)
library(chron)
library(ggplot2)
library(sqldf)
library(tseries)
library(forecast)

## read the data
setwd("C:/Users/tpurohit/Desktop/AV/ts")
train <- fread("train.csv", integer64 = "numeric", header = T)
test <- fread("test.csv", integer64 = "numeric", header = T)
sample_result <- fread("sample_submission.csv", integer64 = "numeric", header = T)

train <- data.frame(train)
test <- data.frame(test)

##do date conversions
train2 <-  train
train2$Datetime<- format(as_datetime(strptime(train2$Datetime, format='%d-%m-%Y %H:%M', tz = "UTC")),
                         format = "%Y-%m-%d %H:%M:%S")
train2$Datetime <-  as_datetime(train2$Datetime)
train2$dt<- format(as_datetime(strptime(train2$Datetime, format='%d-%m-%Y %H:%M', tz = "UTC")),
                   format = "%Y-%m-%d %H:%M:%S")
train2$dt<-as.POSIXlt(train2$dt)
train2$month<- month(train2$dt)
train2$year<- year(train2$dt)
train2$quarter<- quarter(train2$dt)
train2$hours<- hour(train2$dt)
train2$weekday<- weekdays(train2$dt)
train2$weekend <- ifelse(train2$weekday %in% c("Sunday", "Saturday") , 1,0)
train2$dt <-  as.Date(train2$d)
train2$weekday <-  as.numeric(as.factor(train2$weekday))
#for the purpose of ease make the date eilds as null. Many algo cannot handle date 
train2$dt <- NULL
train2$Datetime <- NULL

#apply FSA
library(data.table)
library(dplyr)
library(mlr)
library(Amelia)
library(MASS)
library(sqldf)
library(RSQLite)
library(randomForest)
library(glmnet)
library(xgboost)
library(nnet)
library(caret)
library(FactoMineR)

fnl_dataset <- FSA(data = train1, 
                   dependent.variable = "Count", variable.not.required = "Datetime",
                   depend.variable.type = "continuous")


fsa_subset_data <- data.frame(fnl_dataset[[1]])
fsa_feature_list_summary <- data.frame(fnl_dataset[[2]])
fsa_feature_list_full <- data.frame(fnl_dataset[[3]])

attach(train2)
#apply modelling
model_lm <-  lm(train2$Count~month+year+quarter+hours+weekday+weekend  , data = train2)
summary(model_lm)

model_rf <- randomForest(train2$Count~month+year+quarter+hours+weekday+weekend ,
                         data=train2 , mtry = 5 , ntree= 100)
t1<- data.matrix(train2)
t1 <- t1[, 3:8]
t1
model_xgb <- xgboost(data = t1[,2:6], label = t1[,1],nrounds = 1000,  objective= "reg:linear",
                     eval_metric ="rmse", eta = 0.1)


#---------------------------------------------------------------------------------------------
test1<- test
head(test1$Datetime)

test1$dt<- format(as_datetime(strptime(test1$Datetime, format='%d-%m-%Y %H:%M', tz = "UTC")),
                  format = "%Y-%m-%d %H:%M:%S")

#test1$dt <- format(x, format = "%Y-%m-%d %H:%M:%S")
test1$dt<-as.POSIXlt(test1$dt)
test1$month<- month(test1$dt)
test1$year<- year(test1$dt)
test1$quarter<- quarter(test1$dt)
test1$hours<- hour(test1$dt)
test1$weekday<- weekdays(test1$dt)
test1$weekend <- ifelse(test1$weekday %in% c("Sunday", "Saturday") , 1,0)
test1$dt <-  as.Date(test1$d)
test1$weekday <-  as.numeric(as.factor(test1$weekday))
test1$dt <- NULL
test1$Count<- NULL

test1$count<- predict(model_lm, test1)
write.csv(test1, "teslm.csv")


test1$count<- predict(model_rf, test1)
write.csv(test1, "tesrf.csv")

tst1<- data.matrix(test1)
tst1<- tst1[,3:8]

test1$count<- predict(model_xgb, tst1)
write.csv(test1, "testxgb.csv")

