library(lubridate)
rm(list=ls())

calc.te<-function(a,p){
  n<-length(a)
  p[p<0]=0
  p=round(p)
  error=(log(p+1)-log(a+1))^2
  return(mean(error))
}

test.model<-function(model,test,...){
  i<-which(colnames(test)=="count")
  pred <- predict(model, newdata=test[,-i],...)
  act<-test[,i]
  return(calc.te(act,pred))
}

test.output<-function(model,test,...){
  test_clean<-prep.data(test)
  pred<-round(predict(model,newdata=test_clean,...))
  pred[pred<0]=0
  submit<- data.frame(datetime = test$datetime, count=pred)
  write.csv(submit, file="submit_lh.csv",row.names=FALSE)
}
prep.data<-function(data){
  data$weather<-as.factor(data$weather)
  data$holiday<-as.factor(data$holiday)
  data$workingday<- as.factor(data$workingday)
  data$season<-as.factor(data$season)
  data$datetime<-as.character(data$datetime)
  data$datetime<-strptime(data$datetime, format ='%Y-%m-%d %H:%M:%S')
  
  #create date variable
  data$hour <- as.factor(hour(data$datetime))
  data$day <-day(data$datetime)
  data$month <- month(data$datetime)
  data$year <-year(data$datetime)
  data$weekday<-as.factor(wday(data$datetime))
  return(data)
}


#set working directory
setwd('/Users/linahu/Documents/Developer/Bike\ Sharing/')

data<-read.csv("train.csv")
validate <-read.csv("test.csv")

#clean data
data<-prep.data(data)

#split the training set into training and validation
flag <- data$day>17
train <-data[!flag,]
test<-data[flag,]

#exploratory data analysis
library(ggplot2)
# plot A:total daily count overtime
daily<-aggregate(count~year*month*day,data=train,FUN=sum)
daily$date<-as.Date(ISOdate(daily$year,daily$month,daily$day))
ggplot(daily, aes(date, count)) + geom_line() + xlab("") + ylab("Daily Count")
# plot B:average day hour
day_hour_counts <- aggregate(count~weekday*hour, data=train, mean)
#other plots
qplot(hour, count, data=train, facets=workingday ~.)
qplot(hour, count, data=train, facets=weekday ~.)
qplot(temp, count, data=train, facets=workingday ~.)
qplot(weather, count, data=train, facets=workingday ~.)
qplot(humidity, count, data=train, facets=workingday ~.)
qplot(windspeed, count, data=train, facets=workingday ~.)

#todo:
#generate a variable for daily 
#create a plot for:a. different weather; 

#correlation between variables
cor(train$count,train[,6:17])
ggplot(day_hour_counts, aes(x = hour, y = weekday)) + geom_tile(aes(fill = count)) + scale_fill_gradient(name="Average Counts", low="white", high="red") + theme(axis.title.y = element_blank())

#linear models with different time parameters and interaction terms
lm1<-lm(count~holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train )
lm2<-lm(count~datetime+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train)
lm3<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train)
lm4<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed+holiday*season+workingday*season+weather*holiday, data=train)

# to do:
# partition data based on categorical fields
# add best selection models

#decision tree
library(rpart)
myFormula <- count ~ hour+weekday+year+month+holiday+season+weather+temp+humidity+windspeed
model.rpart <- rpart(myFormula, data = train,control = rpart.control(minsplit = 10))

#random forest
library(randomForest)
model.rf <- randomForest(myFormula, data=train, ntree=100, proximity=TRUE)
test.model(model.rf,test)
#poisson destribution
model.poisson<-glm(myFormula, family="poisson", data=train)

model.linear<-lm(count ~ hour + weekday + year + month + holiday + season + weather + 
                   +                      temp + humidity + windspeed+weekday*hour,data=train)
#boosting
library(gbm)
model.boost=gbm(myFormula,data=train,distribution= "gaussian",n.trees=5000, interaction.depth=6)
test.model(model.boost,test,n.trees=5000)

#final model to submit
#model.rf.submit <- randomForest(myFormula, data=data, ntree=100, proximity=TRUE)
#model.boost.submit=gbm(myFormula,data=data,distribution= "gaussian",n.trees=5000, interaction.depth=6)
#test.output(model.boost.submit,validate,n.trees=5000)
#test.output(model.rf.submit,validate)
#time series
library(xts)
#<<<<<<< HEAD
#train.xts<-as.xts(x=train[,"count"],order.by=train[,"datetime"])
#plot(train.xts[(1:100),])
#f<-decompose(train.ts)  
#par(mfrow=c(2,1))
#acf(train.ts)
#pacf(train.ts)


>>>>>>> FETCH_HEAD
