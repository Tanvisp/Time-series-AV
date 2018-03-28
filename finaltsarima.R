library(data.table)
library(lubridate)
library(Smisc)
library(timeDate)
library(chron)
library(ggplot2)
library(sqldf)
library(tseries)
library(forecast)
library(vars)


setwd("C:/Users/tpurohit/Desktop/AV/ts")
train <- fread("train.csv", integer64 = "numeric", header = T)
test <- fread("test.csv", integer64 = "numeric", header = T)
sample_result <- fread("sample_submission.csv", integer64 = "numeric", header = T)


train <- data.frame(train)
test <- data.frame(test)
train1<- train

#selecting frequency.
#on seeing the data we get that period is: start=c(2012,08,25), end = c(2014, 09, 25) 
#so lets plot the data for freq = 27*7 weekly hours.
train1$ID <- rep(1:168, length.out= nrow(train1))
train1$MainID <- rep(1:168, each = 168)[1:nrow(train1)]

windows()
ggplot(data = train1, aes(x = ID, y = Count, color = factor(MainID))) + geom_line() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  ) + scale_x_continuous(breaks = seq(1,168,1))

#on seeing the plot we clearly see that there exist some pattern over hours distributed over week 
#and hence we confirn the selected freq is correct
#create ts object
tt<- ts(train1$Count,start=c(2012,08,25), end = c(2014, 09, 25) ,frequency=24*7)
plot(tt)
#stationarity test
adf.test(tt,  alternative = "stationary")
kpss.test(tt, null = c("T"), lshort =T)
#both above test says the series is stationary. lets assume it and go ahed 

#analysing acf and pacf
windows()
tt %>% ggtsdisplay(main="Before Differencing")
#the plot did not help us decide on any pdq parameters. lets difference it now
#diff over lag 1
windows()
tt %>%  diff() %>% ggtsdisplay(main="After one Differencing")
#still we dont see any expected pattern in acf and pacf lets diff it with lag of 168
windows()
tt %>% diff(lag = 168) %>% diff() %>% ggtsdisplay(main="After 2 order Differencing")
acf(tt)
pacf(tt)

windows()
acf(tt %>% diff(lag = 168) %>% diff())
windows()
pacf(tt %>% diff(lag = 168) %>% diff())

#we select diff with lag of 168 and the 1 diff

#apply arima
#defining the train with some more refressors
train1$dt<- format(as_datetime(strptime(train1$Datetime, format='%d-%m-%Y %H:%M', tz = "UTC")),
                   format = "%Y-%m-%d %H:%M:%S")
#train1$dt <- format(x, format = "%Y-%m-%d %H:%M:%S")
train1$dt<-as.POSIXlt(train1$dt)
train1$month<- month(train1$dt)
train1$year<- year(train1$dt)
train1$quarter<- quarter(train1$dt)
train1$hours<- hour(train1$dt)
train1$weekday<- weekdays(train1$dt)
train1$weekend <- ifelse(train1$weekday %in% c("Sunday", "Saturday") , 1,0)
train1$dt <-  as.Date(train1$d)
train1$weekday <-  as.numeric(as.factor(train1$weekday))
train1$dt <- NULL
train1$Dat <- NULL
train2<- as.matrix(train1[ 5:10])
arm<-arima(train1[,3], xreg=train2 ,order=c(0,1,3),  seasonal = c(0,1,0))

##creating test 

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

test2<-  as.matrix(test1[,3:8])
x<-predict(arm, n.ahead = 5112, newxreg = test2)
x<- as.data.frame(x)
write.csv(x ,"tstarm.csv")
