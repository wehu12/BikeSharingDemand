library(lubridate)

#clean data
bikerental<-read.table(file="/Users/linahu/Documents/Developer/Bike\ Sharing/train.csv",sep=",",header=TRUE);
summary(bikerental)
bikerental$weather<-as.factor(bikerental$weather)
bikerental$holiday<-as.factor(bikerental$holiday)
bikerental$workingday<- as.factor(bikerental$workingday)
bikerental$season<-as.factor(bikerental$season)
bikerental$datetime<-as.character(bikerental$datetime)
bikerental$datetime<-strptime(bikerental$datetime, format ='%Y-%m-%d %H:%M:%S')

#create date variable
bikerental$hour = hour(bikerental$datetime)

#todo:
# split the training/test data (use the last 1 day of each month of the training set as the test data)

#linear model with or without datetime
lm1<-lm(count~holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=bikerental )
lm2<-lm(count~datetime+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=bikerental)
lm3<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=bikerental)
# linear model with interaction terms
lm4<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed+holiday*season+workingday*season+weather*holiday, data=bikerental)


# to do:
# partition data based on categorical fields

#time series
library(xts)
bikerentalxts<-as.xts(x=bikerental[,"count"],order.by=bikerental[,"datetime"])
plot(bikerentalxts[(1:100),])


