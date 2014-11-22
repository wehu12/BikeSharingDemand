bikerental<-read.table(file="/Users/linahu/Documents/Developer/Bike\ Sharing/train.csv",sep=",",header=TRUE);
summary(bikerental)
attach(bikerental)
bikerental['weather']<-as.factor(bikerental['weather'])
weather<-as.factor(weather)
holiday<-as.factor(holiday)
workingday<- as.factor(workingday)
season<-as.factor(season)
datetime<-as.character(datetime)
datetime<-strptime(datetime, format ='%Y-%m-%d %H:%M:%S')
bikerental<-cbind(datetime,holiday,season,weather,workingday,bikerental[,(6:12)])
#create date variable
library(lubridate)
bikerental$hour = hour(bikerental$datetime)

#todo:
# split the training/test data (use the last 1 day of each month of the training set as the test data)

#linear model with or without datetime
model1<-lm(count~holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=bikerental )
model1<-lm(count~datetime+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=bikerental)
model1<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=bikerental)

# to do:
# dummy variables for time period (morning, night, afternoon)
# interaction terms(not much effect)
# partition data based on categorical fields

#time series
library(xts)
bikerentalxts<-as.xts(x=bikerental[,"count"],order.by=bikerental[,"datetime"])
plot(bikerentalxts[(1:100),])
  


#
